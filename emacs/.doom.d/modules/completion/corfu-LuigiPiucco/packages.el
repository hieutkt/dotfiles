;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :recipe (:files ("*.el" "extensions/*.el")) :pin "cc244c54b392ce4d27ed166730efde4f4d01a07f")
(package! cape :pin "4506ee82129a8e9ff9e4650a9b16eb8c4ae355f3")
(when (modulep! +icons)
  (package! nerd-icons-completion :pin "c2db8557a3c1a9588d111f8c8e91cae96ee85010"))
(when (modulep! +orderless)
  (package! orderless :pin "e6784026717a8a6a7dcd0bf31fd3414f148c542e"))
(when (modulep! :os tty)
  (package! corfu-terminal :pin "501548c3d51f926c687e8cd838c5865ec45d03cc"))
(when (modulep! :editor snippets)
  (package! yasnippet-capf :pin "40654214db7a44db3a99321447632b43a10fae57"))
