#!/bin/bash

if [ "$1" ]; then
    echo "Warning: ignoring any extra arguments: $@"
fi

if [ -e "run-tests.sh" ] ; then
    echo "run-tests.sh found, activating auto test runs."
    AUTO_TEST_FLAG="--auto-test --test-cmd ./run-tests.sh"
fi

# ~$ uv tool uninstall cecli-dev
# ~$ uv tool install --python python3.12 --editable ~/Documents/src/cecli-fork

which cecli
#      --no-fancy-input --no-pretty \
# With dumb terminal, --watch-files does not work (unless in fork)
# Without dumb terminal, the spinner messes up the text
# export
# TERM=dumb
#
# If it hangs on startup with weird file descriptor messages, apply patch:
#
#     interruptible_input.py
#     ```python
#     <<<<<<< SEARCH
#             self._cancel = threading.Event()
#             self._sel = selectors.DefaultSelector()
#     =======
#             self._cancel = threading.Event()
#
#             # On macOS, the default KqueueSelector cannot handle pipe-based stdin
#             # (e.g. when running inside Emacs comint-mode). Fall back to SelectSelector
#             # which works with any file descriptor that supports select().
#             if not sys.stdin.isatty():
#                 self._sel = selectors.SelectSelector()
#             else:
#                 self._sel = selectors.DefaultSelector()
#     >>>>>>> REPLACE
#     ```
#
# To implement --no-spinner:
#
#     Add a new `--spinner` / `--no-spinner` boolean option (default: True) that controls whether the spinner and its associated status text (e.g. "Waiting for …", "Processing…", "Updating repo map", "Compacting…") are shown while waiting for LLM responses.
#
#     When `--no-spinner` is used:
#     - All calls to `self.io.start_spinner(...)`, `self.io.update_spinner(...)`, and `self.io.update_spinner_suffix(...)` in `coders/base_coder.py` must be skipped. Gate each call behind `nested.getter(self.args, "spinner", True)`.
#     - The cost/model information that would normally be shown in the spinner text (the "Waiting for {model} • ${cost} session" string in `send_message`) should instead be saved to `self._deferred_cost_text` and printed once via `self.io.tool_output()` just before
#     the next user prompt — in both `_run_linear` and `input_task` (parallel mode), right after announcements are shown.
#
#     Changes required across files:
#
#     1. **`args.py`**: Add a `--spinner` argument using `argparse.BooleanOptionalAction`, default `True`, in the "Output settings" group, right after the `--stream` argument. Help text: `"Enable/disable the spinner while waiting for LLM responses (default: True)"`.
#
#     2. **`coders/base_coder.py`**: Gate every `self.io.start_spinner(...)`, `self.io.update_spinner(...)`, and `self.io.update_spinner_suffix(...)` call behind `if nested.getter(self.args, "spinner", True):`. In `send_message`, when spinner is disabled, store the
#     cost text in `self._deferred_cost_text` instead of calling `start_spinner`. In `_run_linear` and `input_task`, after showing announcements and before recreating input, flush `self._deferred_cost_text` via `self.io.tool_output()` and reset it to `None`.
#
#     3. **`main.py`**: No changes needed (spinner arg is already read from `args` by the coder).
#
#     4. **`website/docs/config/options.md`**: Add a `### --spinner` entry after `--stream`, documenting the default (`True`), the environment variable (`CECLI_SPINNER`), and the aliases (`--spinner`, `--no-spinner`).
#
#     5. **`website/docs/config/aider_conf.md`**: Add a commented-out `#spinner: true` entry after `#stream: true` in the sample YAML config.
#
#     6. **repo.py**: Also handle this line: "Generating commit message with"
#
# TERM=dumb cecli                               # OK -> implicit --no-fancy-input --pretty
# TERM=ansi cecli --fancy-input --pretty        # Seems to work but then quickly hangs and just acts very weidly
# TERM=ansi cecli --no-fancy-input --pretty     # Sometimes cost lines interspersed..., but "/ask Where is the "powered by" menu item generated?" nicely executes grep commands
# TERM=ansi cecli --no-fancy-input --no-pretty  # OK, but "/ask Where is the "powered by" menu item generated?" does not execute grep commands?
# TERM=ansi cecli --no-fancy-input --no-pretty --no-spinner   # Pretty good!
# TERM=ansi cecli --no-fancy-input --pretty --no-spinner # Tool prompts seem to hang...
TERM=ansi cecli --no-fancy-input --no-pretty --no-spinner \
      --no-tui \
      --watch-files --subtree-only \
      --thinking-tokens 8k --show-thinking \
      --disable-playwright --disable-scraping \
      --architect --auto-accept-architect --yes-always $AUTO_TEST_FLAG

# 2>&1 | tee /tmp/cecli.log.$$

# Works well...
#
# which aider
# TERM=dumb aider --no-fancy-input --no-pretty \
#       --watch-files --subtree-only \
#       --thinking-tokens 8k \
#       --disable-playwright \
#       --architect --auto-accept-architect --yes-always $AUTO_TEST_FLAG

# usage: cecli [-h] [--model MODEL] [--openai-api-key OPENAI_API_KEY]
#              [--anthropic-api-key ANTHROPIC_API_KEY]
#              [--openai-api-base OPENAI_API_BASE]
#              [--set-env ENV_VAR_NAME=value] [--api-key PROVIDER=KEY]
#              [--list-models MODEL] [--model-settings-file MODEL_SETTINGS_FILE]
#              [--model-metadata-file MODEL_METADATA_FILE] [--alias ALIAS:MODEL]
#              [--model-overrides MODEL_OVERRIDES_JSON]
#              [--model-overrides-file MODEL_OVERRIDES_FILE]
#              [--reasoning-effort REASONING_EFFORT]
#              [--thinking-tokens THINKING_TOKENS]
#              [--show-thinking | --no-show-thinking]
#              [--verify-ssl | --no-verify-ssl] [--timeout TIMEOUT]
#              [--edit-format EDIT_FORMAT] [--ask] [--architect] [--agent]
#              [--auto-accept-architect | --no-auto-accept-architect]
#              [--weak-model WEAK_MODEL] [--editor-model EDITOR_MODEL]
#              [--editor-edit-format EDITOR_EDIT_FORMAT]
#              [--show-model-warnings | --no-show-model-warnings]
#              [--check-model-accepts-settings | --no-check-model-accepts-settings]
#              [--max-chat-history-tokens MAX_CHAT_HISTORY_TOKENS]
#              [--retries RETRIES_JSON] [--custom CUSTOM_JSON]
#              [--tui | --no-tui] [--tui-config TUI_CONFIG_JSON]
#              [--agent-config AGENT_CONFIG_JSON] [--auto-save | --no-auto-save]
#              [--auto-save-session-name AUTO_SAVE_SESSION_NAME]
#              [--auto-load | --no-auto-load] [--mcp-servers MCP_CONFIG_JSON]
#              [--mcp-servers-file MCP_CONFIG_FILE]
#              [--mcp-transport MCP_TRANSPORT] [--preserve-todo-list]
#              [--use-enhanced-map] [--security-config SECURITY_CONFIG_JSON]
#              [--enable-context-compaction | --no-enable-context-compaction]
#              [--context-compaction-max-tokens CONTEXT_COMPACTION_MAX_TOKENS]
#              [--context-compaction-summary-tokens CONTEXT_COMPACTION_SUMMARY_TOKENS]
#              [--cache-prompts | --no-cache-prompts]
#              [--cache-keepalive-pings CACHE_KEEPALIVE_PINGS]
#              [--map-tokens MAP_TOKENS]
#              [--map-refresh {auto,always,files,manual}]
#              [--map-multiplier-no-files MAP_MULTIPLIER_NO_FILES]
#              [--map-max-line-length MAP_MAX_LINE_LENGTH]
#              [--map-cache-dir MAP_CACHE_DIR] [--map-memory-cache]
#              [--input-history-file INPUT_HISTORY_FILE]
#              [--chat-history-file CHAT_HISTORY_FILE]
#              [--terminal-setup | --no-terminal-setup] [--dark-mode]
#              [--light-mode] [--pretty | --no-pretty] [--stream | --no-stream]
#              [--user-input-color USER_INPUT_COLOR]
#              [--tool-output-color TOOL_OUTPUT_COLOR]
#              [--tool-error-color TOOL_ERROR_COLOR]
#              [--tool-warning-color TOOL_WARNING_COLOR]
#              [--assistant-output-color ASSISTANT_OUTPUT_COLOR] [--show-speed]
#              [--completion-menu-color COLOR]
#              [--completion-menu-bg-color COLOR]
#              [--completion-menu-current-color COLOR]
#              [--completion-menu-current-bg-color COLOR]
#              [--code-theme CODE_THEME] [--show-diffs] [--git | --no-git]
#              [--gitignore | --no-gitignore]
#              [--add-gitignore-files | --no-add-gitignore-files]
#              [--cecli-ignore CECLI_IGNORE] [--subtree-only]
#              [--auto-commits | --no-auto-commits]
#              [--dirty-commits | --no-dirty-commits]
#              [--attribute-author | --no-attribute-author]
#              [--attribute-committer | --no-attribute-committer]
#              [--attribute-commit-message-author | --no-attribute-commit-message-author]
#              [--attribute-commit-message-committer | --no-attribute-commit-message-committer]
#              [--attribute-co-authored-by | --no-attribute-co-authored-by]
#              [--git-commit-verify | --no-git-commit-verify] [--commit]
#              [--commit-prompt PROMPT] [--dry-run | --no-dry-run]
#              [--skip-sanity-check-repo] [--watch-files | --no-watch-files]
#              [--lint] [--lint-cmd LINT_CMD] [--auto-lint | --no-auto-lint]
#              [--test-cmd TEST_CMD] [--auto-test | --no-auto-test] [--test]
#              [--just-check-update] [--check-update | --no-check-update]
#              [--show-release-notes | --no-show-release-notes] [--upgrade]
#              [--version] [--message COMMAND] [--message-file MESSAGE_FILE]
#              [--copy-paste | --no-copy-paste] [--apply FILE]
#              [--apply-clipboard-edits] [--exit] [--show-repo-map]
#              [--show-prompts] [--linear-output | --no-linear-output] [--debug]
#              [--voice-format VOICE_FORMAT] [--voice-language VOICE_LANGUAGE]
#              [--voice-input-device VOICE_INPUT_DEVICE] [--tweak-responses]
#              [--yes-always] [--yes-always-commands] [--disable-playwright]
#              [--disable-scraping] [--file FILE] [--read FILE] [--vim]
#              [--chat-language CHAT_LANGUAGE]
#              [--commit-language COMMIT_LANGUAGE] [-v] [--load LOAD_FILE]
#              [--encoding ENCODING] [--line-endings {platform,lf,crlf}]
#              [-c CONFIG_FILE] [--env-file ENV_FILE]
#              [--suggest-shell-commands | --no-suggest-shell-commands]
#              [--fancy-input | --no-fancy-input] [--multiline | --no-multiline]
#              [--notifications | --no-notifications]
#              [--notifications-command COMMAND]
#              [--command-prefix COMMAND_PREFIX]
#              [--detect-urls | --no-detect-urls] [--editor EDITOR]
#              [--shell-completions SHELL]
#              [FILE ...]
#
# cecli is AI pair programming in your terminal
#
# options:
#   -h, --help            show this help message and exit
#
# Main model:
#   FILE                  files to edit with an LLM (optional)
#   --model MODEL         Specify the model to use for the main chat [env var:
#                         CECLI_MODEL]
#
# API Keys and settings:
#   --openai-api-key OPENAI_API_KEY
#                         Specify the OpenAI API key [env var:
#                         CECLI_OPENAI_API_KEY]
#   --anthropic-api-key ANTHROPIC_API_KEY
#                         Specify the Anthropic API key [env var:
#                         CECLI_ANTHROPIC_API_KEY]
#   --openai-api-base OPENAI_API_BASE
#                         Specify the api base url [env var:
#                         CECLI_OPENAI_API_BASE]
#   --set-env ENV_VAR_NAME=value
#                         Set an environment variable (to control API settings,
#                         can be used multiple times) [env var: CECLI_SET_ENV]
#   --api-key PROVIDER=KEY
#                         Set an API key for a provider (eg: --api-key
#                         provider=<key> sets PROVIDER_API_KEY=<key>) [env var:
#                         CECLI_API_KEY]
#
# Model settings:
#   --list-models MODEL, --models MODEL
#                         List known models which match the (partial) MODEL name
#                         [env var: CECLI_LIST_MODELS]
#   --model-settings-file MODEL_SETTINGS_FILE
#                         Specify a file with cecli model settings for unknown
#                         models [env var: CECLI_MODEL_SETTINGS_FILE]
#   --model-metadata-file MODEL_METADATA_FILE
#                         Specify a file with context window and costs for
#                         unknown models [env var: CECLI_MODEL_METADATA_FILE]
#   --alias ALIAS:MODEL   Add a model alias (can be used multiple times) [env
#                         var: CECLI_ALIAS]
#   --model-overrides MODEL_OVERRIDES_JSON
#                         Specify model tag overrides directly as JSON/YAML
#                         string (e.g., '{"gpt-4o": {"high": {"temperature":
#                         0.8}}}') [env var: CECLI_MODEL_OVERRIDES]
#   --model-overrides-file MODEL_OVERRIDES_FILE
#                         Specify a file with model tag overrides (e.g.,
#                         gpt-4o:high -> reasoning_effort: high) [env var:
#                         CECLI_MODEL_OVERRIDES_FILE]
#   --reasoning-effort REASONING_EFFORT
#                         Set the reasoning_effort API parameter (default: not
#                         set) [env var: CECLI_REASONING_EFFORT]
#   --thinking-tokens THINKING_TOKENS
#                         Set the thinking token budget for models that support
#                         it. Use 0 to disable. (default: not set) [env var:
#                         CECLI_THINKING_TOKENS]
#   --show-thinking, --no-show-thinking
#                         Show reasoning content in the response (default: True)
#                         [env var: CECLI_SHOW_THINKING]
#   --verify-ssl, --no-verify-ssl
#                         Verify the SSL cert when connecting to models
#                         (default: True) [env var: CECLI_VERIFY_SSL]
#   --timeout TIMEOUT     Timeout in seconds for API calls (default: None) [env
#                         var: CECLI_TIMEOUT]
#   --edit-format EDIT_FORMAT, --chat-mode EDIT_FORMAT
#                         Specify what edit format the LLM should use (default
#                         depends on model) [env var: CECLI_EDIT_FORMAT]
#   --ask                 Use ask edit format for the main chat [env var:
#                         CECLI_ASK]
#   --architect           Use architect edit format for the main chat [env var:
#                         CECLI_ARCHITECT]
#   --agent               Use agent edit format for the main chat (autonomous
#                         file management) [env var: CECLI_AGENT]
#   --auto-accept-architect, --no-auto-accept-architect
#                         Enable/disable automatic acceptance of architect
#                         changes (default: True) [env var:
#                         CECLI_AUTO_ACCEPT_ARCHITECT]
#   --weak-model WEAK_MODEL
#                         Specify the model to use for commit messages and chat
#                         history summarization (default depends on --model)
#                         [env var: CECLI_WEAK_MODEL]
#   --editor-model EDITOR_MODEL
#                         Specify the model to use for editor tasks (default
#                         depends on --model) [env var: CECLI_EDITOR_MODEL]
#   --editor-edit-format EDITOR_EDIT_FORMAT
#                         Specify the edit format for the editor model (default:
#                         depends on editor model) [env var:
#                         CECLI_EDITOR_EDIT_FORMAT]
#   --show-model-warnings, --no-show-model-warnings
#                         Only work with models that have meta-data available
#                         (default: True) [env var: CECLI_SHOW_MODEL_WARNINGS]
#   --check-model-accepts-settings, --no-check-model-accepts-settings
#                         Check if model accepts settings like
#                         reasoning_effort/thinking_tokens (default: True) [env
#                         var: CECLI_CHECK_MODEL_ACCEPTS_SETTINGS]
#   --max-chat-history-tokens MAX_CHAT_HISTORY_TOKENS
#                         Soft limit on tokens for chat history, after which
#                         summarization begins. If unspecified, defaults to the
#                         model's max_chat_history_tokens. [env var:
#                         CECLI_MAX_CHAT_HISTORY_TOKENS]
#   --retries RETRIES_JSON
#                         Specify LLM retry configuration as a JSON string [env
#                         var: CECLI_RETRIES]
#
# Customization Settings:
#   --custom CUSTOM_JSON  Specify cecli customizations configurations (for
#                         prompts, commands, etc.) as a JSON string [env var:
#                         CECLI_CUSTOM]
#
# TUI Settings:
#   --tui, --no-tui       Launch Textual TUI interface [env var: CECLI_TUI]
#   --tui-config TUI_CONFIG_JSON
#                         Specify TUI Mode configuration as a JSON string [env
#                         var: CECLI_TUI_CONFIG]
#
# Agent Settings:
#   --agent-config AGENT_CONFIG_JSON
#                         Specify Agent Mode configuration as a JSON string [env
#                         var: CECLI_AGENT_CONFIG]
#   --auto-save, --no-auto-save
#                         Enable/disable automatic saving of sessions as --auto-
#                         save-session-name (default: False) [env var:
#                         CECLI_AUTO_SAVE]
#   --auto-save-session-name AUTO_SAVE_SESSION_NAME
#                         Specify session name for auto-save and auto-load
#                         (default: auto-save) [env var:
#                         CECLI_AUTO_SAVE_SESSION_NAME]
#   --auto-load, --no-auto-load
#                         Enable/disable automatic loading of --auto-save-
#                         session-name session on startup (default: False) [env
#                         var: CECLI_AUTO_LOAD]
#   --mcp-servers MCP_CONFIG_JSON
#                         Specify MCP server configurations as a JSON string
#                         [env var: CECLI_MCP_SERVERS]
#   --mcp-servers-file MCP_CONFIG_FILE
#                         Specify a file path with MCP server configurations
#                         [env var: CECLI_MCP_SERVERS_FILE]
#   --mcp-transport MCP_TRANSPORT
#                         Specify the transport for MCP servers (default: stdio)
#                         [env var: CECLI_MCP_TRANSPORT]
#   --preserve-todo-list  Deprecated: no longer needed because the todo list is
#                         saved and restored with sessions. This flag has no
#                         effect and will be removed. [env var:
#                         CECLI_PRESERVE_TODO_LIST]
#   --use-enhanced-map    Use enhanced Repo Map that takes into account imports
#                         (default: False) [env var: CECLI_USE_ENHANCED_MAP]
#
# Security Settings:
#   --security-config SECURITY_CONFIG_JSON
#                         Specify Security configuration as a JSON string (e.g.,
#                         '{"allowed-domains": ["github.com"]}') [env var:
#                         CECLI_SECURITY_CONFIG]
#
# Context Compaction:
#   --enable-context-compaction, --no-enable-context-compaction
#                         Enable automatic compaction of chat history to
#                         conserve tokens (default: False) [env var:
#                         CECLI_ENABLE_CONTEXT_COMPACTION]
#   --context-compaction-max-tokens CONTEXT_COMPACTION_MAX_TOKENS
#                         The maximum number of tokens in the conversation
#                         before context compaction is triggered. (default: 80%
#                         of model's context window) [env var:
#                         CECLI_CONTEXT_COMPACTION_MAX_TOKENS]
#   --context-compaction-summary-tokens CONTEXT_COMPACTION_SUMMARY_TOKENS
#                         The target maximum number of tokens for the generated
#                         summary. (default: 4096) [env var:
#                         CECLI_CONTEXT_COMPACTION_SUMMARY_TOKENS]
#
# Cache settings:
#   --cache-prompts, --no-cache-prompts
#                         Enable caching of prompts (default: False) [env var:
#                         CECLI_CACHE_PROMPTS]
#   --cache-keepalive-pings CACHE_KEEPALIVE_PINGS
#                         Number of times to ping at 5min intervals to keep
#                         prompt cache warm (default: 0) [env var:
#                         CECLI_CACHE_KEEPALIVE_PINGS]
#
# Repomap settings:
#   --map-tokens MAP_TOKENS
#                         Suggested number of tokens to use for repo map, use 0
#                         to disable [env var: CECLI_MAP_TOKENS]
#   --map-refresh {auto,always,files,manual}
#                         Control how often the repo map is refreshed. Options:
#                         auto, always, files, manual (default: auto) [env var:
#                         CECLI_MAP_REFRESH]
#   --map-multiplier-no-files MAP_MULTIPLIER_NO_FILES
#                         Multiplier for map tokens when no files are specified
#                         (default: 2) [env var: CECLI_MAP_MULTIPLIER_NO_FILES]
#   --map-max-line-length MAP_MAX_LINE_LENGTH
#                         Maximum line length for the repo map code. Prevents
#                         sending crazy long lines of minified JS files etc.
#                         (default: 100) [env var: CECLI_MAP_MAX_LINE_LENGTH]
#   --map-cache-dir MAP_CACHE_DIR
#                         Directory for the repository map cache
#                         .cecli.tags.cache.v# (default: current directory) [env
#                         var: CECLI_MAP_CACHE_DIR]
#   --map-memory-cache    Store repo map in memory (default: False) [env var:
#                         CECLI_MAP_MEMORY_CACHE]
#
# History Files:
#   --input-history-file INPUT_HISTORY_FILE
#                         Specify the chat input history file (default:
#                         /Users/ekkehard.kraemer/bin/.cecli/input.history) [env
#                         var: CECLI_INPUT_HISTORY_FILE]
#   --chat-history-file CHAT_HISTORY_FILE
#                         Specify the chat history file (default:
#                         /Users/ekkehard.kraemer/bin/.cecli/chat.history) [env
#                         var: CECLI_CHAT_HISTORY_FILE]
#
# Input settings:
#   --terminal-setup, --no-terminal-setup
#                         Auto-configure terminal emulator for shift+enter
#                         support for new lines (default: False) [env var:
#                         CECLI_TERMINAL_SETUP]
#
# Output settings:
#   --dark-mode           Use colors suitable for a dark terminal background
#                         (default: False) [env var: CECLI_DARK_MODE]
#   --light-mode          Use colors suitable for a light terminal background
#                         (default: False) [env var: CECLI_LIGHT_MODE]
#   --pretty, --no-pretty
#                         Enable/disable pretty, colorized output (default:
#                         True) [env var: CECLI_PRETTY]
#   --stream, --no-stream
#                         Enable/disable streaming responses (default: True)
#                         [env var: CECLI_STREAM]
#   --user-input-color USER_INPUT_COLOR
#                         Set the color for user input (default: #00cc00) [env
#                         var: CECLI_USER_INPUT_COLOR]
#   --tool-output-color TOOL_OUTPUT_COLOR
#                         Set the color for tool output (default: None) [env
#                         var: CECLI_TOOL_OUTPUT_COLOR]
#   --tool-error-color TOOL_ERROR_COLOR
#                         Set the color for tool error messages (default:
#                         #FF2222) [env var: CECLI_TOOL_ERROR_COLOR]
#   --tool-warning-color TOOL_WARNING_COLOR
#                         Set the color for tool warning messages (default:
#                         #FFA500) [env var: CECLI_TOOL_WARNING_COLOR]
#   --assistant-output-color ASSISTANT_OUTPUT_COLOR
#                         Set the color for assistant output (default: #0088ff)
#                         [env var: CECLI_ASSISTANT_OUTPUT_COLOR]
#   --show-speed          Show token processing and generation speed in usage
#                         report (default: False) [env var: CECLI_SHOW_SPEED]
#   --completion-menu-color COLOR
#                         Set the color for the completion menu (default:
#                         terminal's default text color) [env var:
#                         CECLI_COMPLETION_MENU_COLOR]
#   --completion-menu-bg-color COLOR
#                         Set the background color for the completion menu
#                         (default: terminal's default background color) [env
#                         var: CECLI_COMPLETION_MENU_BG_COLOR]
#   --completion-menu-current-color COLOR
#                         Set the color for the current item in the completion
#                         menu (default: terminal's default background color)
#                         [env var: CECLI_COMPLETION_MENU_CURRENT_COLOR]
#   --completion-menu-current-bg-color COLOR
#                         Set the background color for the current item in the
#                         completion menu (default: terminal's default text
#                         color) [env var:
#                         CECLI_COMPLETION_MENU_CURRENT_BG_COLOR]
#   --code-theme CODE_THEME
#                         Set the markdown code theme (default: default, other
#                         options include monokai, solarized-dark, solarized-
#                         light, or a Pygments builtin style, see
#                         https://pygments.org/styles for available themes) [env
#                         var: CECLI_CODE_THEME]
#   --show-diffs          Show diffs when committing changes (default: False)
#                         [env var: CECLI_SHOW_DIFFS]
#
# Git settings:
#   --git, --no-git       Enable/disable looking for a git repo (default: True)
#                         [env var: CECLI_GIT]
#   --gitignore, --no-gitignore
#                         Enable/disable adding .cecli* to .gitignore (default:
#                         True) [env var: CECLI_GITIGNORE]
#   --add-gitignore-files, --no-add-gitignore-files
#                         Enable/disable the addition of files listed in
#                         .gitignore to cecli's editing scope. [env var:
#                         CECLI_ADD_GITIGNORE_FILES]
#   --cecli-ignore CECLI_IGNORE
#                         Specify the cecli ignore file (default: .cecli.ignore
#                         in git root) [env var: CECLI_CECLI_IGNORE]
#   --subtree-only        Only consider files in the current subtree of the git
#                         repository [env var: CECLI_SUBTREE_ONLY]
#   --auto-commits, --no-auto-commits
#                         Enable/disable auto commit of LLM changes (default:
#                         True) [env var: CECLI_AUTO_COMMITS]
#   --dirty-commits, --no-dirty-commits
#                         Enable/disable commits when repo is found dirty
#                         (default: True) [env var: CECLI_DIRTY_COMMITS]
#   --attribute-author, --no-attribute-author
#                         Attribute cecli code changes in the git author name
#                         (default: True). If explicitly set to True, overrides
#                         --attribute-co-authored-by precedence. [env var:
#                         CECLI_ATTRIBUTE_AUTHOR]
#   --attribute-committer, --no-attribute-committer
#                         Attribute cecli commits in the git committer name
#                         (default: True). If explicitly set to True, overrides
#                         --attribute-co-authored-by precedence for cecli edits.
#                         [env var: CECLI_ATTRIBUTE_COMMITTER]
#   --attribute-commit-message-author, --no-attribute-commit-message-author
#                         Prefix commit messages with 'cecli: ' if cecli
#                         authored the changes (default: False) [env var:
#                         CECLI_ATTRIBUTE_COMMIT_MESSAGE_AUTHOR]
#   --attribute-commit-message-committer, --no-attribute-commit-message-committer
#                         Prefix all commit messages with 'cecli: ' (default:
#                         False) [env var:
#                         CECLI_ATTRIBUTE_COMMIT_MESSAGE_COMMITTER]
#   --attribute-co-authored-by, --no-attribute-co-authored-by
#                         Attribute cecli edits using the Co-authored-by trailer
#                         in the commit message (default: True). If True, this
#                         takes precedence over default --attribute-author and
#                         --attribute-committer behavior unless they are
#                         explicitly set to True. [env var:
#                         CECLI_ATTRIBUTE_CO_AUTHORED_BY]
#   --git-commit-verify, --no-git-commit-verify
#                         Enable/disable git pre-commit hooks with --no-verify
#                         (default: False) [env var: CECLI_GIT_COMMIT_VERIFY]
#   --commit              Commit all pending changes with a suitable commit
#                         message, then exit [env var: CECLI_COMMIT]
#   --commit-prompt PROMPT
#                         Specify a custom prompt for generating commit messages
#                         [env var: CECLI_COMMIT_PROMPT]
#   --dry-run, --no-dry-run
#                         Perform a dry run without modifying files (default:
#                         False) [env var: CECLI_DRY_RUN]
#   --skip-sanity-check-repo
#                         Skip the sanity check for the git repository (default:
#                         False) [env var: CECLI_SKIP_SANITY_CHECK_REPO]
#   --watch-files, --no-watch-files
#                         Enable/disable watching files for ai coding comments
#                         (default: False) [env var: CECLI_WATCH_FILES]
#
# Fixing and committing:
#   --lint                Lint and fix provided files, or dirty files if none
#                         provided [env var: CECLI_LINT]
#   --lint-cmd LINT_CMD   Specify lint commands to run for different languages,
#                         eg: "python: flake8 --select=..." (can be used
#                         multiple times) [env var: CECLI_LINT_CMD]
#   --auto-lint, --no-auto-lint
#                         Enable/disable automatic linting after changes
#                         (default: True) [env var: CECLI_AUTO_LINT]
#   --test-cmd TEST_CMD   Specify command to run tests [env var: CECLI_TEST_CMD]
#   --auto-test, --no-auto-test
#                         Enable/disable automatic testing after changes
#                         (default: False) [env var: CECLI_AUTO_TEST]
#   --test                Run tests, fix problems found and then exit [env var:
#                         CECLI_TEST]
#
# Upgrading:
#   --just-check-update   Check for updates and return status in the exit code
#                         [env var: CECLI_JUST_CHECK_UPDATE]
#   --check-update, --no-check-update
#                         Check for new cecli versions on launch [env var:
#                         CECLI_CHECK_UPDATE]
#   --show-release-notes, --no-show-release-notes
#                         Show release notes on first run of new version
#                         (default: None, ask user) [env var:
#                         CECLI_SHOW_RELEASE_NOTES]
#   --upgrade, --update   Upgrade cecli to the latest version from PyPI [env
#                         var: CECLI_UPGRADE]
#   --version             Show the version number and exit
#
# Modes:
#   --message COMMAND, --msg COMMAND, -m COMMAND
#                         Specify a single message to send the LLM, process
#                         reply then exit (disables chat mode) [env var:
#                         CECLI_MESSAGE]
#   --message-file MESSAGE_FILE, -f MESSAGE_FILE
#                         Specify a file containing the message to send the LLM,
#                         process reply, then exit (disables chat mode) [env
#                         var: CECLI_MESSAGE_FILE]
#   --copy-paste, --no-copy-paste
#                         Enable automatic copy/paste of chat between cecli and
#                         web UI (default: False) [env var: CECLI_COPY_PASTE]
#   --apply FILE          Apply the changes from the given file instead of
#                         running the chat (debug) [env var: CECLI_APPLY]
#   --apply-clipboard-edits
#                         Apply clipboard contents as edits using the main
#                         model's editor format [env var:
#                         CECLI_APPLY_CLIPBOARD_EDITS]
#   --exit                Do all startup activities then exit before accepting
#                         user input (debug) [env var: CECLI_EXIT]
#   --show-repo-map       Print the repo map and exit (debug) [env var:
#                         CECLI_SHOW_REPO_MAP]
#   --show-prompts        Print the system prompts and exit (debug) [env var:
#                         CECLI_SHOW_PROMPTS]
#   --linear-output, --no-linear-output
#                         Run input and output sequentially instead of us
#                         simultaneous streams (default: None) [env var:
#                         CECLI_LINEAR_OUTPUT]
#   --debug               Turn on verbose debugging (default: False) [env var:
#                         CECLI_DEBUG]
#
# Voice settings:
#   --voice-format VOICE_FORMAT
#                         Audio format for voice recording (default: wav). webm
#                         and mp3 require ffmpeg [env var: CECLI_VOICE_FORMAT]
#   --voice-language VOICE_LANGUAGE
#                         Specify the language for voice using ISO 639-1 code
#                         (default: auto) [env var: CECLI_VOICE_LANGUAGE]
#   --voice-input-device VOICE_INPUT_DEVICE
#                         Specify the input device name for voice recording [env
#                         var: CECLI_VOICE_INPUT_DEVICE]
#
# Other settings:
#   --tweak-responses     Allow manual edits to model responses (default: False)
#                         [env var: CECLI_TWEAK_RESPONSES]
#   --yes-always, --yes   Always say yes to every confirmation (not including
#                         cli commands) [env var: CECLI_YES_ALWAYS]
#   --yes-always-commands, --yolo
#                         Always say yes to every confirmation (including cli
#                         commands) [env var: CECLI_YES_ALWAYS_COMMANDS]
#   --disable-playwright  Never prompt for or attempt to install Playwright for
#                         web scraping (default: False). [env var:
#                         CECLI_DISABLE_PLAYWRIGHT]
#   --disable-scraping    Disable automatic url scraping entirely web scraping
#                         (default: False). [env var: CECLI_DISABLE_SCRAPING]
#   --file FILE           specify a file to edit (can be used multiple times,
#                         glob patterns supported) [env var: CECLI_FILE]
#   --read FILE           specify a read-only file (can be used multiple times,
#                         glob patterns supported) [env var: CECLI_READ]
#   --vim                 Use VI editing mode in the terminal (default: False)
#                         [env var: CECLI_VIM]
#   --chat-language CHAT_LANGUAGE
#                         Specify the language to use in the chat (default:
#                         None, uses system settings) [env var:
#                         CECLI_CHAT_LANGUAGE]
#   --commit-language COMMIT_LANGUAGE
#                         Specify the language to use in the commit message
#                         (default: None, user language) [env var:
#                         CECLI_COMMIT_LANGUAGE]
#   -v, --verbose         Enable verbose output [env var: CECLI_VERBOSE]
#   --load LOAD_FILE      Load and execute /commands from a file on launch [env
#                         var: CECLI_LOAD]
#   --encoding ENCODING   Specify the encoding for input and output (default:
#                         utf-8) [env var: CECLI_ENCODING]
#   --line-endings {platform,lf,crlf}
#                         Line endings to use when writing files (default:
#                         platform) [env var: CECLI_LINE_ENDINGS]
#   -c CONFIG_FILE, --config CONFIG_FILE
#                         Specify the config file (default: search for
#                         .cecli.conf.yml in git root, cwd or home directory)
#                         [env var: CECLI_CONFIG_FILE]
#   --env-file ENV_FILE   Specify the .env file to load (default: .env in git
#                         root) [env var: CECLI_ENV_FILE]
#   --suggest-shell-commands, --no-suggest-shell-commands
#                         Enable/disable suggesting shell commands (default:
#                         True) [env var: CECLI_SUGGEST_SHELL_COMMANDS]
#   --fancy-input, --no-fancy-input
#                         Enable/disable fancy input with history and completion
#                         (default: True) [env var: CECLI_FANCY_INPUT]
#   --multiline, --no-multiline
#                         Enable/disable multi-line input mode with Meta-Enter
#                         to submit (default: False) [env var: CECLI_MULTILINE]
#   --notifications, --no-notifications
#                         Enable/disable terminal bell notifications when LLM
#                         responses are ready (default: False) [env var:
#                         CECLI_NOTIFICATIONS]
#   --notifications-command COMMAND
#                         Specify a command to run for notifications instead of
#                         the terminal bell. If not specified, a default command
#                         for your OS may be used. [env var:
#                         CECLI_NOTIFICATIONS_COMMAND]
#   --command-prefix COMMAND_PREFIX
#                         Specify a command prefix for all commands (useful for
#                         sandboxing) [env var: CECLI_COMMAND_PREFIX]
#   --detect-urls, --no-detect-urls
#                         Enable/disable detection and offering to add URLs to
#                         chat (default: True) [env var: CECLI_DETECT_URLS]
#   --editor EDITOR       Specify which editor to use for the /editor command
#                         [env var: CECLI_EDITOR]
#   --shell-completions SHELL
#                         Print shell completion script for the specified SHELL
#                         and exit. Supported shells: bash, tcsh, zsh. Example:
#                         cecli --shell-completions bash [env var:
#                         CECLI_SHELL_COMPLETIONS]
#
# Args that start with '--' can also be set in a config file
# (/Users/ekkehard.kraemer/bin/.cecli.conf.yml or
# /Users/ekkehard.kraemer/.cecli.conf.yml or specified via -c). The config file
# uses YAML syntax and must represent a YAML 'mapping' (for details, see
# http://learn.getgrav.org/advanced/yaml). In general, command-line values
# override environment variables which override config file values which
# override defaults.

# usage: aider [-h] [--model MODEL] [--openai-api-key OPENAI_API_KEY]
#              [--anthropic-api-key ANTHROPIC_API_KEY]
#              [--openai-api-base OPENAI_API_BASE]
#              [--openai-api-type OPENAI_API_TYPE]
#              [--openai-api-version OPENAI_API_VERSION]
#              [--openai-api-deployment-id OPENAI_API_DEPLOYMENT_ID]
#              [--openai-organization-id OPENAI_ORGANIZATION_ID]
#              [--set-env ENV_VAR_NAME=value] [--api-key PROVIDER=KEY]
#              [--list-models MODEL] [--model-settings-file MODEL_SETTINGS_FILE]
#              [--model-metadata-file MODEL_METADATA_FILE] [--alias ALIAS:MODEL]
#              [--reasoning-effort REASONING_EFFORT]
#              [--thinking-tokens THINKING_TOKENS]
#              [--verify-ssl | --no-verify-ssl] [--timeout TIMEOUT]
#              [--edit-format EDIT_FORMAT] [--architect]
#              [--auto-accept-architect | --no-auto-accept-architect]
#              [--weak-model WEAK_MODEL] [--editor-model EDITOR_MODEL]
#              [--editor-edit-format EDITOR_EDIT_FORMAT]
#              [--show-model-warnings | --no-show-model-warnings]
#              [--check-model-accepts-settings | --no-check-model-accepts-settings]
#              [--max-chat-history-tokens MAX_CHAT_HISTORY_TOKENS]
#              [--cache-prompts | --no-cache-prompts]
#              [--cache-keepalive-pings CACHE_KEEPALIVE_PINGS]
#              [--map-tokens MAP_TOKENS]
#              [--map-refresh {auto,always,files,manual}]
#              [--map-multiplier-no-files MAP_MULTIPLIER_NO_FILES]
#              [--input-history-file INPUT_HISTORY_FILE]
#              [--chat-history-file CHAT_HISTORY_FILE]
#              [--restore-chat-history | --no-restore-chat-history]
#              [--llm-history-file LLM_HISTORY_FILE] [--dark-mode]
#              [--light-mode] [--pretty | --no-pretty] [--stream | --no-stream]
#              [--user-input-color USER_INPUT_COLOR]
#              [--tool-output-color TOOL_OUTPUT_COLOR]
#              [--tool-error-color TOOL_ERROR_COLOR]
#              [--tool-warning-color TOOL_WARNING_COLOR]
#              [--assistant-output-color ASSISTANT_OUTPUT_COLOR]
#              [--completion-menu-color COLOR]
#              [--completion-menu-bg-color COLOR]
#              [--completion-menu-current-color COLOR]
#              [--completion-menu-current-bg-color COLOR]
#              [--code-theme CODE_THEME] [--show-diffs] [--git | --no-git]
#              [--gitignore | --no-gitignore]
#              [--add-gitignore-files | --no-add-gitignore-files]
#              [--aiderignore AIDERIGNORE] [--subtree-only]
#              [--auto-commits | --no-auto-commits]
#              [--dirty-commits | --no-dirty-commits]
#              [--attribute-author | --no-attribute-author]
#              [--attribute-committer | --no-attribute-committer]
#              [--attribute-commit-message-author | --no-attribute-commit-message-author]
#              [--attribute-commit-message-committer | --no-attribute-commit-message-committer]
#              [--attribute-co-authored-by | --no-attribute-co-authored-by]
#              [--git-commit-verify | --no-git-commit-verify] [--commit]
#              [--commit-prompt PROMPT] [--dry-run | --no-dry-run]
#              [--skip-sanity-check-repo] [--watch-files | --no-watch-files]
#              [--lint] [--lint-cmd LINT_CMD] [--auto-lint | --no-auto-lint]
#              [--test-cmd TEST_CMD] [--auto-test | --no-auto-test] [--test]
#              [--analytics | --no-analytics]
#              [--analytics-log ANALYTICS_LOG_FILE] [--analytics-disable]
#              [--analytics-posthog-host ANALYTICS_POSTHOG_HOST]
#              [--analytics-posthog-project-api-key ANALYTICS_POSTHOG_PROJECT_API_KEY]
#              [--just-check-update] [--check-update | --no-check-update]
#              [--show-release-notes | --no-show-release-notes]
#              [--install-main-branch] [--upgrade] [--version]
#              [--message COMMAND] [--message-file MESSAGE_FILE]
#              [--gui | --no-gui | --browser | --no-browser]
#              [--copy-paste | --no-copy-paste] [--apply FILE]
#              [--apply-clipboard-edits] [--exit] [--show-repo-map]
#              [--show-prompts] [--voice-format VOICE_FORMAT]
#              [--voice-language VOICE_LANGUAGE]
#              [--voice-input-device VOICE_INPUT_DEVICE] [--disable-playwright]
#              [--file FILE] [--read FILE] [--vim]
#              [--chat-language CHAT_LANGUAGE]
#              [--commit-language COMMIT_LANGUAGE] [--yes-always] [-v]
#              [--load LOAD_FILE] [--encoding ENCODING]
#              [--line-endings {platform,lf,crlf}] [-c CONFIG_FILE]
#              [--env-file ENV_FILE]
#              [--suggest-shell-commands | --no-suggest-shell-commands]
#              [--fancy-input | --no-fancy-input] [--multiline | --no-multiline]
#              [--notifications | --no-notifications]
#              [--notifications-command COMMAND]
#              [--detect-urls | --no-detect-urls] [--editor EDITOR]
#              [--shell-completions SHELL] [--opus] [--sonnet] [--haiku] [--4]
#              [--4o] [--mini] [--4-turbo] [--35turbo] [--deepseek] [--o1-mini]
#              [--o1-preview]
#              [FILE ...]
