(cond ((= (x-display-pixel-height) 1440)
       (add-to-list 'default-frame-alist
		    '(font . "Victor Mono-18")))
      (t
       (add-to-list 'default-frame-alist
		    '(font . "Victor Mono-16"))))
