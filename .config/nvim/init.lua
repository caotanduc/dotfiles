vim.opt.clipboard = "unnamedplus"
vim.opt.fixendofline = true
vim.opt.cursorline = true
vim.opt.colorcolumn = "80"
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.showtabline = 0

vim.keymap.set("n", "<Esc>", function()
    vim.cmd("nohlsearch")
end)

vim.g.mapleader = " "

vim.o.statusline = table.concat({
  "%f ",
  "%m %r ",
  "%=",
  "Ln: %l ",
})

vim.pack.add({
    { src = "https://github.com/stevearc/oil.nvim" },
    { src = "https://github.com/ibhagwan/fzf-lua" },
    { src = "https://github.com/echasnovski/mini.pairs" },
    { src = "https://github.com/nvim-lua/plenary.nvim" }, -- required
    { src = "https://github.com/NeogitOrg/neogit" },
    { src = "https://github.com/projekt0n/github-nvim-theme" }
})

vim.cmd.colorscheme('github_dark_colorblind')

require("neogit").setup()
vim.keymap.set("n", "<leader>gg", "<cmd>Neogit<CR>")

require("mini.pairs").setup({
    modes = { insert = true, command = false, terminal = false },
})

require("fzf-lua").setup({
  -- UI
  winopts = {
    height = 0.5,
    width = 1.0,
    row = 1.0,
    col = 0.50,
    border = "none",
    preview = {
        hidden = "hidden"
    },
  },

  -- fzf behavior
  fzf_opts = {
    ["--layout"] = "reverse", -- prompt on top (like Emacs minibuffer)
  },

  -- files
  files = {
    prompt = "Files❯ ",
    git_icons = true,
    file_icons = true,
    color_icons = true,
    hidden = true, -- show dotfiles
  },

  -- grep
  grep = {
    prompt = "Grep❯ ",
  },

  -- buffers
  buffers = {
    prompt = "Buffers❯ ",
    sort_lastused = true,
  },

  -- keymaps inside fzf
  keymap = {
    builtin = {
      ["<C-j>"] = "down",
      ["<C-k>"] = "up",
      ["<C-q>"] = "select-all+accept",
    },
    fzf = {
      ["ctrl-j"] = "down",
      ["ctrl-k"] = "up",
    },
  },
})

--------------------------------------------------
-- Project root (like project.el)
--------------------------------------------------
local function project_root()
  return vim.fs.root(0, {
    ".git",
    "package.json",
    "pyproject.toml",
    "Cargo.toml",
    "go.mod",
  }) or vim.loop.cwd()
end

--------------------------------------------------
-- Keybindings (project.el style)
--------------------------------------------------

-- project prefix
vim.keymap.set("n", "<leader>p", "<Nop>")

-- project-find-file
vim.keymap.set("n", "<leader>pf", function()
  require("fzf-lua").files({ cwd = project_root() })
end, { desc = "Project files" })

-- project-find-regexp
vim.keymap.set("n", "<leader>pg", function()
  require("fzf-lua").live_grep({ cwd = project_root() })
end, { desc = "Project grep" })

-- project-switch-to-buffer
vim.keymap.set("n", "<leader>pb", function()
  require("fzf-lua").buffers()
end, { desc = "Buffers" })

-- project-recent-files
vim.keymap.set("n", "<leader>po", function()
  require("fzf-lua").oldfiles({ cwd = project_root() })
end, { desc = "Recent files" })

-- project root
vim.keymap.set("n", "<leader>pd", function()
  local root = project_root()
  vim.cmd("cd " .. root)
  print("Project root: " .. root)
end, { desc = "Set project root" })

--------------------------------------------------
-- Global useful bindings (like Emacs M-x style)
--------------------------------------------------

-- find file (anywhere)
vim.keymap.set("n", "<leader>ff", "<cmd>FzfLua files<CR>")

-- live grep
vim.keymap.set("n", "<leader>fg", "<cmd>FzfLua live_grep<CR>")

-- buffers
vim.keymap.set("n", "<leader>fb", "<cmd>FzfLua buffers<CR>")

-- help tags
vim.keymap.set("n", "<leader>fh", "<cmd>FzfLua help_tags<CR>")

-- resume last search
vim.keymap.set("n", "<leader>fr", "<cmd>FzfLua resume<CR>")

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

require("oil").setup({
  default_file_explorer = true,

  view_options = {
    show_hidden = true, -- like `-a`
  },

  columns = {
      "permissions",
      "size",
      "mtime"
  },

  -- Dired-like keybindings
  keymaps = {
--     ["<CR>"] = "actions.select",        -- open file
    ["^"] = "actions.parent",           -- go up (like dired)
--     ["-"] = "actions.parent",

--     ["g."] = "actions.toggle_hidden",   -- toggle dotfiles

--     ["q"] = "actions.close",            -- quit (like dired)

--     ["R"] = "actions.rename",           -- rename
--     ["D"] = "actions.delete",           -- delete (like dired D)
--     ["C"] = "actions.copy",             -- copy
--     ["M"] = "actions.move",             -- move (rename)

    ["o"] = "actions.select_vsplit",    -- open other window
--     ["v"] = "actions.select_vsplit",
--     ["s"] = "actions.select_split",

--     ["r"] = "actions.refresh",          -- revert buffer
  },
})

-- open like dired
vim.keymap.set("n", "-", "<CMD>Oil<CR>")
