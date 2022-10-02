(cond ((= (x-display-pixel-height) 1440)
       (add-to-list 'default-frame-alist
		    '(font . "VictorMono Nerd Font-16")))
      (t
       (add-to-list 'default-frame-alist
		    '(font . "VictorMono Nerd Font-12"))))
