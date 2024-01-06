;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "24dccafeea114b1aec7118f2a8405b46aa0051e0")
(package! cape :pin "18a30f48bb8754421cb10dad99e0a406173d4551")
(when (modulep! +icons)
  (package! nerd-icons-corfu :pin "7077bb76fefc15aed967476406a19dc5c2500b3c"))
(when (modulep! +orderless)
  (package! orderless :pin "b24748093b00b37c3a572c4909f61c08fa27504f"))
(when (modulep! :os tty)
  (package! corfu-terminal :pin "501548c3d51f926c687e8cd838c5865ec45d03cc"))
(when (modulep! :editor snippets)
  (package! yasnippet-capf :pin "a0a6b1c2bb6decdad5cf9b74202f0042f494a6ab"))
