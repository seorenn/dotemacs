;; font-lock-warning-face
;; for a construct that is peculiar, or that greatly changes the meaning of other text, like ‘;;;###autoload’ in Emacs Lisp and ‘#error’ in C.
;; font-lock-function-name-face
;; for the name of a function being defined or declared.
;; font-lock-variable-name-face
;; for the name of a variable being defined or declared.
;; font-lock-keyword-face
;; for a keyword with special syntactic significance, like ‘for’ and ‘if’ in C.
;; font-lock-comment-face
;; for comments.
;; font-lock-comment-delimiter-face
;; for comments delimiters, like ‘/*’ and ‘*/’ in C. On most terminals, this inherits from font-lock-comment-face.
;; font-lock-type-face
;; for the names of user-defined data types.
;; font-lock-constant-face
;; for the names of constants, like ‘NULL’ in C.
;; font-lock-builtin-face
;; for the names of built-in functions.
;; font-lock-preprocessor-face
;; for preprocessor commands. This inherits, by default, from font-lock-builtin-face.
;; font-lock-string-face
;; for string constants.
;; font-lock-doc-face
;; for documentation strings in the code. This inherits, by default, from font-lock-string-face.
;; font-lock-negation-char-face
;; for easily-overlooked negation characters.

(setq ksp-keywords
      '(("on\\|end" . font-lock-keyword-face)
        ("\\$ENGINE_PAR_CUTOFF\\|\\$KNOB_UNIT_HZ\\|\\$NI_BUS_OFFSET\\|\\$NI_ASYNC_EXIT_STATUS\\|\\$NI_ASYNC_ID\\|\\$CONTROL_PAR_BASEPATH\\|\\$CONTROL_PAR_FILE_TYPE\\|\\$CONTROL_PAR_COLUMN_WIDTH\\|\\$CONTROL_PAR_HEIGHT\\|\\$CONTROL_PAR_WIDTH" . font-lock-constant-face)
        ("init\\|ui_control\\|async_complete\\|note\\|declare\\|if\\|while\\|select\\|case\\|ui_knob\\|ui_button\\|ui_menu" . font-lock-builtin-face)
        ("set_listener\\|play_note\\|in_range\\|abs\\|inc\\|dec\\|array_equal\\|num_elements\\|search\\|sort\\|ms_to_ticks\\|allow_group\\|disallow_group\\|purge_group\\|find_group\\|group_name\\|exit\\|make_perfview\\|set_script_title\\|message\\|move_control\\|set_text\\|set_knob_unit\\|set_knob_defval\\|get_engine_par_disp\\|set_knob_label\\|make_persistent\\|wait\\|load_midi_file\\|save_midi_file\\|set_control_par_str\\|set_control_par\\|move_control_px\\|mf_set_export_area\\|fs_navigate" . font-lock-function-name-face)
        )
      )

(define-derived-mode ksp-mode fundamental-mode
  (setq font-lock-defaults '(ksp-keywords))
  (setq mode-name "KSP")
)
