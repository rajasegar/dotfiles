;; EMMS
(use-package emms
  :ensure t)

(emms-all)

;; Enable DBUS
(require 'emms-mpris)
(emms-mpris-enable)

(setq emms-player-list '(emms-player-mplayer)
      emms-info-functions '(emms-info-native))


(emms-history-load)
;; (emms-librefm-scrobbler-enable)
