import subprocess
import time
from ast import literal_eval
import base64
import binascii
start_time = time.time()
import json
#subprocess.run(["lein run clojush.problems.demos.tagged-regression-c"])




#print(prog)

prog = r'"(string_pop char_dup_times string_parse_to_chars string_flush boolean_eq boolean_pop string_reverse boolean_empty exec_yankdup integer_eq integer_sub tagged_781 \I boolean_swap string_frominteger string_flush boolean_shove integer_mult integer_add string_replacefirstchar string_pop string_removechar integer_mod integer_lte integer_div char_isletter char_rot char_frominteger char_swap integer_mod exec_swap () (integer_max string_split integer_dec char_dup_items string_swap) integer_flush integer_dup_times string_containschar string_last string_split char_shove tagged_386 string_occurrencesofchar string_shove print_newline print_integer char_eq exec_stackdepth integer_fromchar integer_dup_items \newline integer_dup_times string_parse_to_chars char_allfromstring integer_min char_isdigit in1 string_dup_times string_conjchar boolean_or integer_sub exec_do*range (string_eq string_reverse string_parse_to_chars) string_dup_items exec_do*range (boolean_flush boolean_dup_items string_emptystring exec_swap () (integer_max boolean_swap print_boolean exec_do*while (boolean_invert_first_then_and integer_swap string_fromboolean exec_yank string_dup_items boolean_dup_items)) exec_dup_times (integer_dec string_fromboolean boolean_frominteger string_eq boolean_and exec_s (string_replacechar char_eq exec_dup_times (print_char \Z) string_stackdepth string_replacefirst integer_max) (boolean_pop integer_inc integer_shove) (string_replace exec_flush) string_contains boolean_invert_first_then_and boolean_yankdup) boolean_flush char_rot string_yankdup char_rot string_pop string_replacefirst string_reverse integer_shove char_eq print_string exec_dup_times (integer_mod) char_rot in1 exec_y (integer_fromchar) string_pop exec_when (char_isdigit integer_shove) print_char) exec_shove (char_frominteger tag_char_508) string_shove string_replace boolean_xor exec_dup () string_removechar boolean_xor string_butlast string_last boolean_yankdup string_replacefirst exec_eq exec_yankdup boolean_dup_items boolean_dup_items string_flush exec_y (integer_yankdup integer_sub integer_flush string_reverse tagged_634 integer_fromstring) integer_flush exec_k (string_first exec_swap (exec_do*while (integer_dec) exec_dup (integer_dup integer_lte string_removechar integer_inc) exec_dup_items string_fromchar string_replacechar string_take char_yankdup integer_add integer_dup_times integer_stackdepth string_replacechar string_conjchar string_swap) (integer_gte boolean_or string_flush exec_stackdepth char_yankdup) boolean_rot integer_swap string_replacechar exec_shove () string_indexofchar) (string_dup_items integer_pop boolean_invert_first_then_and print_char \newline string_stackdepth boolean_flush exec_rot (boolean_eq integer_pop integer_rot string_replacefirst boolean_empty integer_swap exec_noop string_length) (char_dup_times string_fromboolean string_concat integer_dec integer_fromstring integer_fromstring string_swap) (string_emptystring integer_dup_items char_dup string_butlast integer_lt string_replacefirstchar)))"'

prog = '''(tagged_728 string_removechar exec_noop integer_rot print_string string_yankdup string_length integer_dec in1 integer_mult integer_dup_items boolean_invert_second_then_and exec_noop print_string integer_div string_removechar integer_empty exec_rot (string_dup) (boolean_dup) (string_parse_to_chars integer_eq boolean_pop char_flush integer_yank integer_dec string_replacefirst exec_do*count () exec_noop boolean_flush) integer_empty boolean_rot integer_pop string_split exec_do*count (char_isletter) integer_lt char_swap string_parse_to_chars boolean_yankdup string_replacefirstchar integer_inc string_swap char_eq string_substring char_eq integer_swap " K&eQ/- T f a ed" exec_empty exec_dup (char_iswhitespace exec_do*while (string_yankdup in1 char_empty string_length boolean_dup) integer_empty exec_stackdepth string_first integer_min exec_yank) boolean_and exec_noop char_empty)'''

#prog = bytearray(prog, 'utf-8')
pro  =json.dumps(prog)

#prog = '"'+ prog.encode('unicode-escape').decode() + '"'
x = subprocess.Popen("lein run " +  pro, shell=True, stdout=subprocess.PIPE)
out, err = x.communicate()

reuse_tuple = literal_eval(out.decode("utf-8").replace(" ",','))[0]
rep_tuple = literal_eval(out.decode("utf-8").replace(" ",','))[1]
print("Resue is ", sum(reuse_tuple)/len(reuse_tuple))
print("Rep is ", sum(rep_tuple)/len(rep_tuple))


print("--- %s seconds ---" % (time.time() - start_time))