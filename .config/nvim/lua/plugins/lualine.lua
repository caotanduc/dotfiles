return {
  'nvim-lualine/lualine.nvim',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  config = function()
    -- Define a palette that roughly matches github_dark_colorblind
    local colors = {
      bg        = '#0d1117',  -- GitHub dark background
      fg        = '#c9d1d9',  -- Foreground
      blue      = '#40a4ff',  -- Colour-blind friendly blue
      orange    = '#ff9d00',  -- Substitute for red/green
      green     = '#3fb950',
      violet    = '#a371f7',
      cyan      = '#76e3ea',
      gray      = '#8b949e',
      darkgray  = '#161b22',
    }

    -- Custom theme definition
    local custom_theme = {
      normal = {
        a = { fg = colors.bg, bg = colors.blue, gui = 'bold' },
        b = { fg = colors.fg, bg = colors.darkgray },
        c = { fg = colors.fg, bg = colors.bg },
      },
      insert = {
        a = { fg = colors.bg, bg = colors.green, gui = 'bold' },
        b = { fg = colors.fg, bg = colors.darkgray },
        c = { fg = colors.fg, bg = colors.bg },
      },
      visual = {
        a = { fg = colors.bg, bg = colors.violet, gui = 'bold' },
        b = { fg = colors.fg, bg = colors.darkgray },
        c = { fg = colors.fg, bg = colors.bg },
      },
      replace = {
        a = { fg = colors.bg, bg = colors.orange, gui = 'bold' },
        b = { fg = colors.fg, bg = colors.darkgray },
        c = { fg = colors.fg, bg = colors.bg },
      },
      inactive = {
        a = { fg = colors.gray, bg = colors.bg, gui = 'bold' },
        b = { fg = colors.gray, bg = colors.bg },
        c = { fg = colors.gray, bg = colors.bg },
      },
    }

    require('lualine').setup({
      options = {
        theme = custom_theme,
        icons_enabled = true,
        component_separators = { left = '', right = '' },
        section_separators = { left = '', right = '' },
        globalstatus = true,
      },
      sections = {
        lualine_a = {},  -- no mode indicator
        lualine_b = {
          { 'branch',   color = { fg = colors.blue,   bg = colors.darkgray } },
          { 'diff',     color = { fg = colors.cyan,   bg = colors.darkgray } },
          { 'diagnostics', color = { fg = colors.orange, bg = colors.darkgray } },
        },
        lualine_c = {
          { 'filename', path = 1, color = { fg = colors.green, bg = colors.bg } },
        },
        lualine_x = {},
        lualine_y = {},
        lualine_z = {
          { 'location', color = { fg = colors.violet, bg = colors.bg } },
        },
      },
    })
  end
}

