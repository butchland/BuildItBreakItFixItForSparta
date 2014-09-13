#include "functions.h"

void tokenOpt(char * input) {
	reti = regcomp(&regex, "[^a-zA-Z0-9]", 0);
	reti = regexec(&regex, input, 0, NULL, 0);
	if (!reti) {
		invalid();
	}
	regfree(&regex);
}

void nameOpt(char * input) {
	reti = regcomp(&regex, "[^a-zA-Z]", 0);
	reti = regexec(&regex, input, 0, NULL, 0);
	if (!reti) {
		invalid();
	}
	regfree(&regex);
}

void numOpt(char * input) {
	reti = regcomp(&regex, "[^0-9]", 0);
	reti = regexec(&regex, input, 0, NULL, 0);
	if (!reti) {
		invalid();
	}
	regfree(&regex);
}

void printMD5(char * toPrint) {
	int i;
	printf("0x");
	for (i = 0; i < MD5_DIGEST_LENGTH; i++)
		printf("%02x", (unsigned char) toPrint[i]);
	printf("%s", "\n");
}

off_t fsize(const char *filename) {
	struct stat st;

	if (stat(filename, &st) == 0)
		return st.st_size;

	return -1;
}

int do_crypt(FILE *in, FILE *out, int do_encrypt, unsigned char *key_data,
		int key_data_len, unsigned char *salt) {
	/* Allow enough space in output buffer for additional block */
	int i, nrounds = 2;
	unsigned char key[16], iv[16];
	unsigned char inbuf[1024], outbuf[1024 + EVP_MAX_BLOCK_LENGTH];
	memset(inbuf, 0, 1024);
	int inlen, outlen;
	EVP_CIPHER_CTX ctx;
	/* Bogus key and IV: we'd normally set these from
	 * another source.
	 */
	i = EVP_BytesToKey(EVP_aes_128_cbc(), EVP_sha1(), salt, key_data,
			key_data_len, nrounds, key, iv);
	if (i != 16) {
		printf("Key size is %d bits - should be 256 bits\n", i);
		return -1;
	}
	/* Don't set key or IV right away; we want to check length */
	EVP_CIPHER_CTX_init(&ctx);
	EVP_CipherInit_ex(&ctx, EVP_aes_128_cbc(), NULL, NULL, NULL, do_encrypt);
	OPENSSL_assert(EVP_CIPHER_CTX_key_length(&ctx) == 16);
	OPENSSL_assert(EVP_CIPHER_CTX_iv_length(&ctx) == 16);
	/* Now we can set key and IV */
	EVP_CipherInit_ex(&ctx, NULL, NULL, key, iv, do_encrypt);
	for (;;) {

		inlen = fread(inbuf, 1, 1024, in);
		if (inlen <= 0)
			break;
		memset(outbuf, 0, 1024 + EVP_MAX_BLOCK_LENGTH);
		if (!EVP_CipherUpdate(&ctx, outbuf, &outlen, inbuf, inlen)) {
			/* Error */
			EVP_CIPHER_CTX_cleanup(&ctx);
			return 0;
		}
		fwrite(outbuf, 1, outlen, out);
		memset(inbuf, 0, 1024);
	}
	if (!EVP_CipherFinal_ex(&ctx, outbuf, &outlen)) {
		/* Error */
		EVP_CIPHER_CTX_cleanup(&ctx);
		fclose(in);
		fclose(out);
		invalid_token();
	}
	fwrite(outbuf, 1, outlen, out);
	EVP_CIPHER_CTX_cleanup(&ctx);
	fclose(in);
	fclose(out);
	return 1;
}

void invalid_check(logread_args * args) {
	printf("invalid\n");
	unsigned int salt[] = { 12345, 54321 };
	FILE * decrypted_file = fopen(args->logName, "r");
	FILE * encrypted = fopen("tempblahman", "w+");
	do_crypt(decrypted_file, encrypted, 1, args->token, strlen(args->token),
			(unsigned char *) salt);
	rename("tempblahman", args->logName);
	exit(-1);
}

void invalid() {
	printf("invalid\n");
	exit(-1);
}

void invalid_0() {
	printf("invalid\n");
	exit(0);
}

void invalid_token() {
	fprintf(stderr, "security error\n");
	int32_t fileSize = fsize("tempblahman");
	if (fileSize > 15)
		unlink("tempblahman");
	exit(-1);
}

