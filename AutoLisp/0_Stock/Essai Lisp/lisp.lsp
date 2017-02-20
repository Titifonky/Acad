(defun c:delGroup ( / )
(setq i 1)
(setq nosgroup '(nil))
(setq namesel (ssget))
(setq lgsel (sslength namesel))
(while (< i lgsel)
((setq entdef (entget (ssname namesel i)))
(if (equal '(102 . "{ACAD_REACTORS") (assoc 102 entdef))
(
(setq groupno (assoc 330 entdef))
(if (not (member groupno nosgroup))
((subst groupno 'nil nosgroup)
(cons 'nil nosgroup))
)
)
)
)
)
(+ i 1)
)