#include <stdio.h>
#include <stdint.h>
#include <dirent.h>
#include <errno.h>

#define MAX_PATH 4096
#define CODE_BUF_SIZE 5000000

struct MaxNesting {
	int list;
};

static void swap_buffers(
	char one[CODE_BUF_SIZE],
	char two[CODE_BUF_SIZE],
	int* one_size,
	int* two_size) {

	uint8_t two_tmp;
	for (int i = 0; i < *one_size; ++i) {
		two_tmp = two[i];
		two[i] = one[i];
		one[i] = two_tmp;
	}

	int one_size_tmp = *one_size;
	*one_size = *two_size;
	*two_size = one_size_tmp;
}

static void make_file_path(
	char file_path[MAX_PATH],
	char directory_path[MAX_PATH],
	char file_name[256]) {

	// copy the directory path
	int i = 0;
	for (; i < MAX_PATH; ++i) {
		if (directory_path[i] == '\0') {
			break;
		}
		file_path[i] = directory_path[i];
	}

	file_path[i] = '/';
	++i;
	
	// copy the file name
	for (int j = 0; j < 256; ++j) {
		if (file_name[j] == '\0') {
			file_path[i+j] = '\0';
			return;
		}

		file_path[i + j] = file_name[j];
	}
	printf("SHOULDNT HAPPEN");
}

static void make_sub_dir_path(
	char sub_dir_path[MAX_PATH],
	char directory_path[MAX_PATH],
	char sub_dir_name[256]) {

	int i = 0;
	for (; i < MAX_PATH; ++i) {
		if (directory_path[i] == '\0') {
			break;
		}

		sub_dir_path[i] = directory_path[i];
	}

	sub_dir_path[i] = '/';
	++i;

	for (int j = 0; j < 256; ++j) {
		if (sub_dir_name[j] == '\0') {
			sub_dir_path[i+j] = '\0';
			return;
		}
		sub_dir_path[i + j] = sub_dir_name[j];
	}
}

static int is_relevant_dir(char dir_name[256]) {
	if (dir_name[0] == '.' && dir_name[1] == '\0') {
		return 0;
	}

	if (
		dir_name[0] == '.' &&
		dir_name[1] == '.' &&
		dir_name[2] == '\0') {

		return 0;
	}

	return 1;
}

static int is_elm_file(char file_name[256]) {
	int size = 0;
	for (; size < 256; ++size) {
		if (file_name[size] == '\0') {
			break;
		}
	}
	
	// smallest Elm file name
	// 12345 size
	// 01234 index
	// X.elm
	return
		size > 4 &&
		file_name[size - 1] == 'm' &&
		file_name[size - 2] == 'l' &&
		file_name[size - 3] == 'e' &&
		file_name[size - 4] == '.';
}


struct CodeBuffers {
	char one[CODE_BUF_SIZE];
	char two[CODE_BUF_SIZE];
	int two_size;
	int one_size;
};

struct CodeBuffers CODE_BUFFERS;

static int is_start_verbatim_string(
	int i,
	char buf[CODE_BUF_SIZE],
	int size) {

	return
		size - i > 3 &&
		buf[i] == '"' &&
		buf[i+1] == '"' &&
		buf[i+2] == '"';
}

static int is_start_normal_string(
	int i,
	char buf[CODE_BUF_SIZE],
	int size) {

	return size - i > 1 && buf[i] == '"';
}

static int is_start_of_type_declaration(int i, char buf[CODE_BUF_SIZE], int size) {
	return
		size - i > 6 &&
		buf[i] == ' ' &&
		buf[i-1] == 'e' &&
		buf[i-2] == 'p' &&
		buf[i-3] == 'y' &&
		buf[i-4] == 't' &&
		buf[i-5] == '\n';
}

static int is_ending_verbatim_string(
	int i,
	char buf[CODE_BUF_SIZE],
	int size) {

	return
		i > 2 &&
		buf[i] == '"' &&
		buf[i-1] == '"' &&
		buf[i-2] == '"' &&
		buf[i-3] != '\\';
}

static int is_ending_normal_string(
	int i,
	char buf[CODE_BUF_SIZE],
	int size) {

	return i > 0 && buf[i] == '"' && buf[i-1] != '\\';
}


static int is_ending_block_comment(int i, char buf[CODE_BUF_SIZE], int size) {
	return i > 0 && buf[i] == '}' && buf[i-1] == '-';
}

static int is_name_char(char ch) {
	return
		(ch >= 'a' && ch <= 'z') ||
		(ch >= 'A' && ch <= 'Z') ||
		(ch >= '0' && ch <= '9') ||
		ch == '_';
}

static int is_ending_let(int i, char buf[CODE_BUF_SIZE], int size) {
	return
		i > 3 &&
		buf[i] == 'n' &&
		buf[i-1] == 'i' &&
		!is_name_char(buf[i-2]) &&
		i+1 < size &&
		(buf[i+1] == ' ' || buf[i+1] == '\n');
}

static int is_ending_type_declaration(int i, char buf[CODE_BUF_SIZE], int size) {
	return i > 1 && buf[i] != ' ' && buf[i-1] == '\n';
}

enum SubRegion {
	Let,
	NotInSubRegion,
	CurlyBracketCollection,
	BracketCollection,
	ParenthesisCollection,
	TypeDeclaration,
};

static inline int end_of_sub_region(
	int i,
	enum SubRegion sub_region,
	int size,
	char buf[CODE_BUF_SIZE]) {

	switch (sub_region) {
	case TypeDeclaration:
		return is_ending_type_declaration(i, buf, size);
	case ParenthesisCollection:
		return buf[i] == ')';
	case BracketCollection:
		return buf[i] == ']';
	case CurlyBracketCollection:
		return buf[i] == '}';
	case Let:
		return is_ending_let(i, buf, size);
	case NotInSubRegion:
		return 0;
	}
}

static int is_start_block_comment(int i, char buf[CODE_BUF_SIZE], int size) {
	return
		size - i > 2 &&
		buf[i] == '{' &&
		buf[i+1] == '-';
}

static int is_start_let(int i, char buf[CODE_BUF_SIZE], int size) {
	return
		i > 0 &&
		size - i > 4 &&
        (buf[i-1] == ' ' || buf[i-1] == '\n') &&
		buf[i] == 'l' &&
		buf[i+1] == 'e' &&
		buf[i+2] == 't' &&
		(buf[i+3] == ' ' || buf[i+3] == '\n');
}

static inline enum SubRegion start_of_sub_region(int i, int size, char buf[CODE_BUF_SIZE]) {
	if (is_start_of_type_declaration(i, buf, size)) {
		return TypeDeclaration;
	}

	if (buf[i] == '[') {
		return BracketCollection;
	}

	if (buf[i] == '{') {
		return CurlyBracketCollection;
	}

	if (buf[i] == '(') {
		return ParenthesisCollection;
	}

	if (is_start_let(i, buf, size)) {
		return Let;
	}

	return NotInSubRegion;
}

static int consume_spaces(char buf[CODE_BUF_SIZE], int i, int size) {
	for (; i < size && buf[i] == ' '; ++i) {}
	return i;
}

static int equals_but_not_at_end_of_line(char buf[CODE_BUF_SIZE], int i, int size) {
	return
		i > 0 &&
		buf[i+1] != '=' &&
		buf[i-1] != '=' &&
		buf[i-1] != '!' &&
		buf[i-1] != '<' &&
		buf[i-1] != '>' &&
		buf[i] == '=' &&
		buf[consume_spaces(buf, i+1, size)] != '\n';
}

static int consume_line_comment(char buf[CODE_BUF_SIZE], int i, int buf_size) {
	if (buf[i] != '-' || i+2 >= buf_size || buf[i+1] != '-') {
		return 0;
	}

	int size = 0;
	for (; buf[i+size] != '\n'; ++size) {
	}

	return size;
}

static int consume_verbatim_string(char buf[CODE_BUF_SIZE], int i, int buf_size) {
	if (!is_start_verbatim_string(i, buf, buf_size)) {
		return 0;
	}

	int size = 3;
	for (; !is_ending_verbatim_string(i+size, buf, buf_size); ++size) {
	}

	return size;
}

static int consume_normal_string(char buf[CODE_BUF_SIZE], int i, int buf_size) {
	if (!is_start_normal_string(i, buf, buf_size)) {
		return 0;
	}

	int size = 1;
	for (; !is_ending_normal_string(i+size, buf, buf_size); ++size) {
	}

	return size;
}

static inline int consume_block_comment(char buf[CODE_BUF_SIZE], int i, int buf_size) {
	if (!is_start_block_comment(i, buf, buf_size)) {
		return 0;
	}

	i += 2;

	int size = 0;
	int nesting = 0;

	for (; size <= buf_size; ++size) {
		if (is_start_block_comment(i+size, buf, buf_size)) {
			++nesting;
			continue;
		}

		if (is_ending_block_comment(i+size, buf, buf_size)) {
			if (nesting == 0) {
				return size;
			}

			--nesting;
		}
	}
}

static inline int consume_ignore(char buf[CODE_BUF_SIZE], int i, int size) {
	int verbatim_size = consume_verbatim_string(buf, i, size);
	if (verbatim_size > 0) {
		return verbatim_size;
	}

	int normal_string_size = consume_normal_string(buf, i, size);
	if (normal_string_size > 0) {
		return normal_string_size;
	}

	int block_comment_size = consume_block_comment(buf, i, size);
	if (block_comment_size > 0) {
		return block_comment_size;
	}

	return consume_line_comment(buf, i, size);
}

static int list_has_newlines(
	char buf[CODE_BUF_SIZE],
	int i,
	int buf_size) {

	int nesting = 0;
	for (; !(nesting == 0 && buf[i] == ']'); ++i) {
		i += consume_ignore(buf, i, buf_size);
		if (buf[i] == '[') {
			++nesting;
		}
		if (buf[i] == ']') {
			--nesting;
		}
		if (buf[i] == '\n') {
			return 1;
		}
	}

	return 0;
}

static void format_list_level(
	char one[CODE_BUF_SIZE],
	char two[CODE_BUF_SIZE],
	int* one_size,
	int* two_size,
	int nesting_level) {

	int nesting = 0;
	int has_newlines = 0;
	int column = 0;
	int start_column = 0;
	int two_i = 0;
	for (int one_i = 0; one_i < *one_size; ++one_i) {
		one_i += consume_ignore(one, one_i, *one_size);
		if (one[one_i] == '[') {
			++nesting;
		}
		if (one[one_i] == '[' && nesting == nesting_level) {
			has_newlines = list_has_newlines(one, one_i+1, *one_size);
			start_column = column;
		}
		if (one[one_i] == ']') {
			--nesting;
		}
		if (one[one_i] == '\n') {
			column = 0;
		} else {
			++column;
		}
		if (one[one_i] == '[' && !has_newlines && nesting == nesting_level) {
			two[two_i] = '[';
			++two_i;

			++one_i;

			two[two_i] = ' ';
			++two_i;

			for (; one[one_i] == ' '; ++one_i) {
			}
		}
		if (one[one_i] == '[' && has_newlines && nesting == nesting_level) {
			two[two_i] = '[';
			++two_i;

			++one_i;

			two[two_i] = ' ';
			++two_i;

			for (; one[one_i] == ' ' || one[one_i] == '\n'; ++one_i) {
			}
		}

		if (one[one_i] == ']' && has_newlines && nesting+1 == nesting_level) {
			int init_two_i = two_i;
			--two_i;
			for (; two[two_i] == ' ' || two[two_i] == '\n'; --two_i) {
			}

			++two_i;

			two[two_i] = '\n';
			++two_i;

			for (int i = 0; i < start_column; ++i) {
				two[two_i] = ' ';
				++two_i;
			}
		}
		if (nesting == nesting_level && one[one_i] == ',' && !has_newlines) {
			--two_i;
			for (; two[two_i] == ' '; --two_i) {
			}

			++two_i;

			two[two_i] = ',';
			++two_i;

			two[two_i] = ' ';
			++two_i;

			++one_i;
			for (; one[one_i] == ' '; ++one_i) {
			}
		}

		if (nesting == nesting_level && one[one_i] == ',' && has_newlines) {
			--two_i;
			for (; two[two_i] == ' ' || two[two_i] == '\n'; --two_i) {
			}

			++two_i;
			two[two_i] = '\n';
			++two_i;

			for (int i = 0; i < start_column; ++i) {
				two[two_i] = ' ';
				++two_i;
			}
			two[two_i] = ',';
			++two_i;
			two[two_i] = ' ';
			++two_i;

			++one_i;
			for (; one[one_i] == ' ' || one[one_i] == '\n'; ++one_i) {
			}
		}

		two[two_i] = one[one_i];
		++two_i;
	}
	*two_size = two_i;
}

static int equal_buffers(
	char one[CODE_BUF_SIZE],
	char two[CODE_BUF_SIZE],
	int one_size,
	int two_size) {

	if (one_size != two_size) {
		return 0;
	}

	for (int i = 0; i < one_size; ++i) {
		if (one[i] != two[i]) {
			return 0;
		}
	}

	return 1;
}

static void format_list_pass(
	char one[CODE_BUF_SIZE],
	char two[CODE_BUF_SIZE],
	int* one_size,
	int* two_size,
	int max_nesting) {

	int i = 0;
	for (; i < max_nesting; ++i) {
		if (i%2 == 0) {
			format_list_level(one, two, one_size, two_size, i+1);
			continue;
		}
		format_list_level(two, one, two_size, one_size, i+1);
	}

	if (i%2 == 1) {
		return;
	}

	swap_buffers(one, two, one_size, two_size);
}


static int max_list_nesting(char one[CODE_BUF_SIZE], int one_size) {
	int max_nesting = 0;
	int nesting = 0;
	for (int i = 0; i < one_size; ++i) {
		i += consume_ignore(one, i, one_size);
		if (one[i] == '[') {
			++nesting;
		}
		if (nesting > max_nesting) {
			max_nesting = nesting;
		}
		if (one[i] == ']') {
			--nesting;
		}
	}
	return max_nesting;
}

static void format_expression(
	char one[CODE_BUF_SIZE],
	char two[CODE_BUF_SIZE],
	int* one_size,
	int* two_size) {

	int max_nesting = max_list_nesting(one, *one_size);

	format_list_pass(one, two, one_size, two_size, max_nesting);

	int i = 0;
	for (; max_nesting > 0 && !equal_buffers(one, two, *one_size, *two_size); ++i) {

		if (i%2 == 0) {
			format_list_pass(two, one, two_size, one_size, max_nesting);
			continue;
		}

		format_list_pass(one, two, one_size, two_size, max_nesting);
	}

	if (i%2 == 0) {
		return;
	}

	swap_buffers(one, two, one_size, two_size);
}

static void toplevel_body_indent(
	char one[CODE_BUF_SIZE],
	char two[CODE_BUF_SIZE],
	int* one_size,
	int* two_size) {

	enum SubRegion sub_region_status = NotInSubRegion;
	int sub_region_nesting = 0;

	int two_i = 0;
	for (int one_i = 0; one_i < *one_size; ++one_i) {
		int comment_size = consume_ignore(one, one_i, *one_size);
		for (int i = 0; i < comment_size; ++i) {
			two[two_i] = one[one_i];
			++one_i;
			++two_i;
		}
		enum SubRegion new_sub_region_status =
			start_of_sub_region(one_i, *one_size, one);

		if (
			sub_region_status != NotInSubRegion &&
			new_sub_region_status == sub_region_status) {

			++sub_region_nesting;
		}

		if (sub_region_status == NotInSubRegion) {
			sub_region_status = new_sub_region_status;
		}

		if (end_of_sub_region(
			one_i,
			sub_region_status,
			*one_size,
			one)) {

			if (sub_region_nesting == 0) {
				sub_region_status = NotInSubRegion;
			} else {
				--sub_region_nesting;
			}
		}

		if (
			sub_region_status == NotInSubRegion &&
			one[one_i] == '=' &&
			one_i < *one_size &&
			one[one_i + 1] != '=' &&
			one_i > 0 &&
			one[one_i - 1] != '=') {

			two[two_i] = '=';
			++two_i;
			two[two_i] = '\n';
			++two_i;

			one_i += 2;

			for (
				;
				one[one_i + 1] == ' ' ||
					one[one_i + 1] == '\n';
				++one_i) {
			}
			for (int i = 0; i < 3; ++i) {
				two[two_i] = ' ';
				++two_i;
			}
		}

		two[two_i] = one[one_i];
		++two_i;
	}
	*two_size = two_i;
}

static void newline_after_toplevel_bind(
	char one[CODE_BUF_SIZE],
	char two[CODE_BUF_SIZE],
	int* one_size,
	int* two_size) {

	enum SubRegion sub_region_status = NotInSubRegion;
	int sub_region_nesting = 0;

	int two_i = 0;
	for (int one_i = 0; one_i < *one_size; ++one_i) {
		int comment_size = consume_ignore(one, one_i, *one_size);
		for (int i = 0; i < comment_size; ++i) {
			two[two_i] = one[one_i];
			++one_i;
			++two_i;
		}

		enum SubRegion new_sub_region_status =
			start_of_sub_region(one_i, *one_size, one);

		if (
			sub_region_status != NotInSubRegion &&
			new_sub_region_status == sub_region_status) {

			++sub_region_nesting;
		}

		if (sub_region_status == NotInSubRegion) {
			sub_region_status = new_sub_region_status;
		}


		if (end_of_sub_region(
			one_i,
			sub_region_status,
			*one_size,
			one)) {

			if (sub_region_nesting == 0) {
				sub_region_status = NotInSubRegion;
			} else {
				--sub_region_nesting;
			}
		}

		if (
			sub_region_status == NotInSubRegion &&
			equals_but_not_at_end_of_line(one, one_i, *one_size)) {

			++one_i;

			two[two_i] = '=';
			++two_i;
			two[two_i] = '\n';
			++two_i;

			for (
				;
				one[one_i + 1] == ' ' ||
					one[one_i + 1] == '\n';
				++one_i) {
			}

			for (int i = 0; i < 4; ++i) {
				two[two_i] = ' ';
				++two_i;
			}
		}

		two[two_i] = one[one_i];
		++two_i;
	}
	*two_size = two_i;
}

static void no_trailing_space(
	char one[CODE_BUF_SIZE],
	char two[CODE_BUF_SIZE],
	int* one_size,
	int* two_size) {

	int num_consecutive_space = 0;

	int two_i = 0;
	for (int one_i = 0; one_i < *one_size; ++one_i) {
		if (one[one_i] == ' ') {
			num_consecutive_space++;
		}

		if (one[one_i] != ' ' && one[one_i] != '\n') {

			num_consecutive_space = 0;
		}

		if (one[one_i] == '\n' && num_consecutive_space > 0) {

			two_i = two_i - num_consecutive_space;
			num_consecutive_space = 0;
		}

		two[two_i] = one[one_i];
		two_i++;
	}

	*two_size = two_i;
}

static int format_file(char path[MAX_PATH]) {
	printf("%s\n", path);

	FILE* handle_in = fopen(path, "rb");
	if (handle_in == NULL) {
		printf("failed to open the file: %s", path);
		return -1;
	}

	size_t n = fread(CODE_BUFFERS.one, 1, CODE_BUF_SIZE, handle_in);
	if (!feof(handle_in)) {
		printf(
			"file too large: %s, max size is %d",
			path,
			CODE_BUF_SIZE);
		fclose(handle_in);
		return -1;
	}
	if (ferror(handle_in)) {
		printf("couldn't open input file: %s", path);
		fclose(handle_in);
		return -1;
	}

	CODE_BUFFERS.one_size = n;

	// Add formatters in here.
	no_trailing_space(
		CODE_BUFFERS.one,
		CODE_BUFFERS.two,
		&CODE_BUFFERS.one_size,
		&CODE_BUFFERS.two_size);
	newline_after_toplevel_bind(
		CODE_BUFFERS.two,
		CODE_BUFFERS.one,
		&CODE_BUFFERS.two_size,
		&CODE_BUFFERS.one_size);
	toplevel_body_indent(
		CODE_BUFFERS.one,
		CODE_BUFFERS.two,
		&CODE_BUFFERS.one_size,
		&CODE_BUFFERS.two_size);
	format_expression(
		CODE_BUFFERS.two,
		CODE_BUFFERS.one,
		&CODE_BUFFERS.two_size,
		&CODE_BUFFERS.one_size);

	FILE* handle_out = fopen(path, "w");
	if (handle_out == NULL) {
		printf("couldn't open output file: %s", path);
		return -1;
	}

	n = fwrite(CODE_BUFFERS.one, 1, CODE_BUFFERS.one_size, handle_out);
	if (n != CODE_BUFFERS.one_size) {
		printf("failed writing output to %s", path);
		fclose(handle_out);
		return -1;
	}
	fclose(handle_out);

	return 0;
}

static int get_paths_from_dir(DIR*, char[MAX_PATH]);

static int get_paths_from_fs_entry(DIR* d, char directory_path[MAX_PATH]) {

	struct dirent* dir = readdir(d);
	if (dir == NULL && errno != 0) {
		printf("couldn't read directory: %s", directory_path);
		return -1;
	}
	if (dir == NULL && errno == 0) {
		return 1;
	}
	if (dir->d_type == DT_REG && !is_elm_file(dir->d_name)) {
		return 0;
	}
	if (dir->d_type == DT_REG) {
		char file_path[MAX_PATH];
		make_file_path(file_path, directory_path, dir->d_name);

		int format_error = format_file(file_path);
		if (format_error != 0) {
			return format_error;
		}
	}
	if (dir->d_type == DT_DIR && !is_relevant_dir(dir->d_name)) {
		return 0;
	}
	if (dir->d_type != DT_DIR) {
		return 0;
	}
	char sub_dir_path[MAX_PATH];
	make_sub_dir_path(sub_dir_path, directory_path, dir->d_name);

	DIR* subd = opendir(sub_dir_path);
	if (subd == NULL) {
		printf("directory doesn't exist: \"%s\"\n", sub_dir_path);
		return -1;
	}
	int result = get_paths_from_dir(subd, sub_dir_path);
	closedir(subd);
	if (result != 0) {
		return result;
	}
	return 0;
}


static int get_paths_from_dir(DIR* d, char directory_path[MAX_PATH]) {

	errno = 0;
	int result = 0;
	while (result == 0) {
		result = get_paths_from_fs_entry(d, directory_path);
	}
	if (result > 0) {
		return 0;
	}

	return result;
}

int main(int argc, char* argv[]) {
	DIR* d;
	char top_path[MAX_PATH];
	top_path[0] = '.';
	top_path[1] = '\0';
	d = opendir(top_path);
	if (d == NULL) {
	printf("directory doesn't exist: %s", top_path);
		return -1;
	}
	int result = get_paths_from_dir(d, top_path);
	closedir(d);
	return result;
}
