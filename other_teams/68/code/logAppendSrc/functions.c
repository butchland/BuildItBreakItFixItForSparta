#include "functions.h"

void tokenOpt(char * input) {
	regcomp(&regex, "[^a-zA-Z0-9]", 0);
	reti = regexec(&regex, input, 0, NULL, 0);
	if (!reti) {
		invalid();
	}
	regfree(&regex);
}

void cryptWrapper(logappend_args * args, int32_t type) {
	uint32_t salt[] = { 12345, 54321 };
	int32_t fileSize = fsize(args->logName);
	if (fileSize < 16 && type == 0)
		return;
	FILE * encrypted_file = fopen(args->logName, "r");
	FILE * decrypted = fopen("tempblahman", "w");
	do_crypt(encrypted_file, decrypted, type, args->token, strlen(args->token),
			(unsigned char *) salt);
	rename("tempblahman", args->logName);
}

void nameOpt(char * input) {
	regcomp(&regex, "[^a-zA-Z]", 0);
	reti = regexec(&regex, input, 0, NULL, 0);
	if (!reti) {
		invalid();
	}
	regfree(&regex);
}

void numOpt(char * input) {
	regcomp(&regex, "[^0-9]", 0);
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

int check_logic(logappend_args * args) {
	char * name = NULL;
	logicUser* newUser = NULL;
	if (args->employeeName == NULL) {
		name = args->guestName;
		newUser = ht_get(allMahHashes_guests, name);

	} else {
		name = args->employeeName;
		newUser = ht_get(allMahHashes_employees, name);
	}

	if (newUser == NULL) {
		newUser = calloc(1, sizeof(logicUser));
		strcpy(newUser->name, name);
		newUser->inBuilding = 0;
		newUser->inRoom = 0;
		newUser->roomID = 0;
		if (args->employeeName != NULL) {
			ht_put(allMahHashes_employees, newUser->name, newUser);
		} else {
			ht_put(allMahHashes_guests, newUser->name, newUser);
		}
	}
	if (args->timestamp <= oldTime) {
		return -1;
	}
	oldTime = args->timestamp;

	logicUser* temp = newUser;

	//LOGIC MATRIX ACTION!!!!
	if (args->eventArrival == 1) {
		if (args->roomID == -1 && !temp->inBuilding) {
			temp->inBuilding = 1;
		} else if (args->roomID == -1 && temp->inBuilding) {
			return -1;
		} else if (args->roomID != -1 && !temp->inBuilding) {
			return -1;
		} else if (args->roomID != -1 && temp->inRoom) {
			return -1;
		} else if (args->roomID != -1 && !temp->inRoom) {
			temp->inRoom = 1;
			temp->roomID = args->roomID;
		}

	} else if (args->eventDeparture == 1) {
		if (args->roomID == -1 && temp->inBuilding) {
			temp->inBuilding = 0;
			temp->roomID = -1;
		} else if (args->roomID == -1 && !temp->inBuilding) {
			return -1;
		} else if (args->roomID != -1 && !temp->inBuilding) {
			return -1;
		} else if (args->roomID != -1 && !temp->inRoom) {
			return -1;
		} else if (args->roomID != -1 && temp->inRoom) {
			if (args->roomID != temp->roomID) {
				return -1;
			}
			temp->inRoom = 0;
			temp->roomID = 0;
		}
	}
	return 1;
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
	/* Don't set key or IV right away; we want to check lengths */
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

void invalid() {
	printf("invalid\n");
	if (isBatch) {
		exit(0);
	} else {
		exit(-1);
	}
}
void invalid_check(logappend_args * args) {
	printf("invalid\n");
	//encrypt
	cryptWrapper(args, 1);
	if (isBatch) {
		exit(0);
	} else {
		exit(-1);
	}
}
void invalid_token() {
	fprintf(stderr, "security error\n");
	int32_t fileSize = fsize("tempblahman");
	if (fileSize > 15)
		unlink("tempblahman");
	exit(-1);
}
