---
title: A new live preview plugin for nvim
date: 2026-11-05
description: ""
tags: nvim
---

# [live-preview.nvim](https://github.com/ailrk/live-preview.nvim) - A new live preview plugin for nvim

There are already so many markdown previewers for nvim, why do you need a new one?

Cursor synchronization

Depends on your workflow, this may or may not be important for you.

You can put markdown editing workflow into two extremes.

One end of the spectrum is what I call a "compiler approach". You do all your edit completely in the editor and only check the rendered markdown once in a while. Like writing C, instead of constantly expecting the program change as we editing the source file, we only check once we're done our changes. Markdown is already designed for text readablility, so it should work fine most of the time. However, if you add any more complicated assets like images, videos,

On the other end, you have full WYSIWYG editors like typora, completely merges the editing and the preview together. The preview is fully dynamic and truly "live". This is very convinent if you need to check the result all the time, but because WYSIWYG application is very complicated, it's hard to make one, and the editing experience is usually not as good as a focused editor.


<!-- a brief overview of markdown format / history -->

<!-- survey existing previewer -->

<!-- the status of previewer -->

<!-- the status of nvim previewer support  -->

<!-- why cursor is so important -->
