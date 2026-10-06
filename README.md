# eudic.el

在 Emacs 中调用欧路词典查词，并通过欧路 OpenAPI 管理生词本。

## 配置

将此目录加入 `load-path` 后加载：

```elisp
(require 'eudic)

;; 可选快捷键
(global-set-key (kbd "C-c e d") #'eudic-lookup)
(global-set-key (kbd "C-c e p") #'eudic-lookup-in-popup)
```

查词需要本机安装欧路词典，并能通过系统打开 `eudic://` 链接；仅查词不需要 API 密钥或 `plz`。

## 查词

- `M-x eudic-lookup`：使用 `eudic://dict/` 打开普通查词。
- `M-x eudic-lookup-in-popup`：使用 `eudic://lp-dict/` 打开欧路的小窗口查词。

两个命令均优先查询选中的文本，其次查询光标处的单词；没有可用文本时提示输入。支持短语和中文，会去除首尾空白并对 URL 编码。也可以通过 Lisp 直接调用，例如 `(eudic-lookup "hello world")`。

默认通过 `browse-url-default-browser` 交给操作系统处理。可设置 `eudic-open-url-function` 为接收一个 URL 参数的函数，定制启动方式。

### 查词时保留 Emacs 焦点（macOS）

```elisp
(setq eudic-activate nil)  ; Emacs Lisp 的假值是 nil
```

`eudic-activate` 默认为 `t`，沿用标准 URL 打开方式和 `eudic-open-url-function`。
设为 `nil` 后，主窗口查词改用异步 AppleScript `show dic with word`，不发送 `activate`，也不调用 URL 打开函数。
查询文本作为独立进程参数传递，支持引号、换行和中文，不会插入脚本代码。

此模式要求 macOS、`osascript` 和应用标识为 `com.eusoft.eudic` 的欧路词典。首次使用可能需要允许 Emacs 自动化控制欧路；执行失败会显示错误，不会回退到可能抢焦点的 URL 方式。
已在欧路 25.9.0 **已运行状态**下实测保留 Emacs 焦点；首次启动行为尚未验证。

**LightPeek 暂不支持此模式。** 主应用的脚本没有小窗口参数，本机 LightPeek 对相同查词事件返回“不支持”错误（`-1708`）。
因此 `eudic-activate` 为 `nil` 时，`eudic-lookup-in-popup` 会报错；gt 请使用 `(eudic-gt-engine :popup nil)`。
需要 LightPeek 时设回 `(setq eudic-activate t)`，窗口激活行为由欧路决定。

## 查词后自动加入生词本

自动加词默认关闭。安装 `plz`（`M-x package-install RET plz RET`）及其运行依赖 `curl`，然后配置：

```elisp
(setq eudic-api-key "NIS 你的授权信息"
      eudic-auto-add-to-studylist t
      eudic-default-studylist-id "0"
      eudic-default-language 'en)
```

在[欧路授权页面](https://my.eudic.net/OpenAPI/Authorization)获取密钥，保存在个人配置中。库不会提供或覆盖你的密钥。

`"0"` 是内置的“我的生词本”；也可以设为其他生词本的 ID。语言支持 `en`、`de`、`es`、`fr`，此设置控制加词请求的语言。接口依据[欧路生词本 API 文档](https://my.eudic.net/OpenAPI/doc_api_study)第 1.6 节，使用 `POST /v1/studylist/words`。

普通查词和小窗口查词共用此设置。发送查词链接后异步加词；不激活模式则在 AppleScript 成功退出后加词，脚本失败不会加词。失败会在 Emacs 中显示提示。自动加词不以“欧路是否查到释义”为条件。设置 `eudic-auto-add-to-studylist` 为 `nil` 即可关闭。

自动加词使用独立的内部函数和 `eudic-default-studylist-id` 设置；原有 `eudic-add-word-to-studylist` 命令及其 `eudic-default-studylist` 设置仍然保留。

## 接入 gt

安装 [gt](https://github.com/lorniu/gt.el) 及其依赖后，加载可选模块 `eudic-gt`。
普通查词和生词本功能不依赖 gt。

```elisp
(require 'gt)
(require 'eudic-gt)

(defvar my-eudic-translator
  (gt-translator
   :taker (gt-taker :langs '(en zh) :text 'word :pick nil)
   :engines (eudic-gt-engine :popup t)
   :render (eudic-gt-render)))

(defun my-gt-eudic ()
  (interactive)
  (let ((gt-polyglot-p nil))
    (gt-start my-eudic-translator)))

(global-set-key (kbd "C-c e g") #'my-gt-eudic)
```

执行 `M-x my-gt-eudic`，gt 获取选区或光标下的单词，然后打开欧路小窗口。
`:popup` 默认为 `t`；设置为 `nil` 使用主窗口。需要确认输入时，可在 taker 中增加 `:prompt t`。
也可以将此 translator 配置为 `gt-default-translator` 或加入 `gt-preset-translators`；使用 `gt-translate` 时也应关闭 `gt-polyglot-p`。

模块提供三个接口：

- `eudic-gt-engine`：调用现有查词函数，沿用 `eudic-activate`、`eudic-open-url-function` 和自动加入生词本设置，默认不启用缓存。
- `eudic-gt-output`：隐藏占位结果，保留调用错误提示。
- `eudic-gt-render`：返回 `(gt-render :output #'eudic-gt-output)`，无需定义新的 renderer 类。

欧路 URL 协议不返回译文；引擎返回一个空字符串占位，让 gt 正常完成任务。这只表示调用已发出，不表示欧路已经查到释义。
不激活模式同样在脚本进程启动后返回占位；稍后的脚本错误直接显示在 Emacs 消息区。
gt 的源语言和目标语言不会传给欧路，也不会覆盖 `eudic-default-language`。
一次只支持一项文本和一个目标语言，不支持分段批量查询或流式输出；保留 `:pick nil`、`:delimit nil`，关闭多目标语言，以免连续查询覆盖欧路窗口。

建议使用独立的欧路 translator：`eudic-gt-render` 会隐藏同一 translator 中所有引擎的结果，不适合直接与需要展示译文的引擎共用。

## 生词本管理

加载 `(require 'eudic-studylist)` 后可使用：

- `M-x eudic-refresh-studylists`：刷新所有支持语言的生词本缓存。
- `M-x eudic-create-studylist`：创建生词本；使用 `C-u` 前缀可选择语言。
- `M-x eudic-delete-studylist`：选择并删除生词本。
- `M-x eudic-add-word-to-studylist`：选择生词本并输入单词，或使用 `eudic-default-studylist` 指定的生词本。该设置支持生词本 ID 和 `"language-default"`（当前语言的默认生词本）。

原有 Lisp 调用 `(eudic-add-word-to-studylist word studylist)` 仍受支持，第二个参数是 `eudic-studylist` 对象，也可以省略。
只想在 gt 取词后加词时，仍可使用原有回调方式：

```elisp
(defun my-add-word-to-eudic (translator)
  (eudic-add-word-to-studylist (car (oref translator text))))

;; 在现有 gt-taker 配置中加入 :then #'my-add-word-to-eudic
```

## 测试

在 `plz` 所在目录已加入 `load-path` 的环境运行：

```sh
emacs -Q --batch -L . -L /path/to/plz -l tests/eudic-test.el -f ert-run-tests-batch-and-exit
```

测试替换系统 URL 启动、AppleScript 进程和 HTTP 传输，不会打开欧路或修改真实生词本。

gt 集成测试使用真实的 gt 和 pdd，仅替换欧路启动函数：

```sh
emacs -Q --batch -L . -L /path/to/gt -L /path/to/pdd -l tests/eudic-gt-test.el -f ert-run-tests-batch-and-exit
```
