(cond ((= (x-display-pixel-height) 1440)
       (add-to-list 'default-frame-alist
		    '(font . "Victor Mono-16")))
      (t
       (add-to-list 'default-frame-alist
		    '(font . "Victor Mono-14"))))
