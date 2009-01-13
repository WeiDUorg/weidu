		READ_2DA_ENTRY tb#pretty_print_indent + 1 0 0 tb#pretty_print_dmp
		SET_2DA_ENTRY_LATER tb#pretty_print_dmp2 tb#pretty_print_indent + 1 0 ~%tb#pretty_print_dmp%~
		SET_2DA_ENTRIES_NOW tb#pretty_print_dmp2 0
		READ_2DA_ENTRY tb#pretty_print_indent 0 0 ~tb#pretty_print_header~
		REPLACE_EVALUATE ~^\(%tb#pretty_print_header%  *\)~ BEGIN
		SPACES somespace ~%MATCH1%~
		END ~%MATCH1%~
		SET_2DA_ENTRY tb#pretty_print_indent 0 0 ~%somespace%%tb#pretty_print_header%~
