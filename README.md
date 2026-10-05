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

普通查词和小窗口查词共用此设置。发送查词链接后异步加词，失败会在 Emacs 中显示提示。URL 协议不会返回释义或查词成功状态，因此自动加词不以“欧路是否查到释义”为条件。设置 `eudic-auto-add-to-studylist` 为 `nil` 即可关闭。

## 生词本管理

加载 `(require 'eudic-studylist)` 后可使用：

- `M-x eudic-refresh-studylists`：刷新所有支持语言的生词本缓存。
- `M-x eudic-create-studylist`：创建生词本；使用 `C-u` 前缀可选择语言。
- `M-x eudic-delete-studylist`：选择并删除生词本。

## 测试

在 `plz` 所在目录已加入 `load-path` 的环境运行：

```sh
emacs -Q --batch -L . -L /path/to/plz -l tests/eudic-test.el -f ert-run-tests-batch-and-exit
```

测试替换系统 URL 启动和 HTTP 传输，不会打开欧路或修改真实生词本。
