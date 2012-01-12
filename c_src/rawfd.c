#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

#include "erl_nif.h"

static ERL_NIF_TERM
rawfd_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char path[1024];
    int fd;
    ERL_NIF_TERM ret;

    if(enif_get_string(env, argv[0], path, 1024, ERL_NIF_LATIN1) <= 0) {
        fprintf(stderr, "Invalid path.\r\n");
        return enif_make_badarg(env);
    }

    fd = open(path, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP);
    if(fd < 0) {
        fprintf(stderr, "Error opening file.\r\n");
        return enif_make_badarg(env);
    }
    ret = enif_make_int(env, fd);

    return enif_make_tuple2(env, enif_make_atom(env, "ok"), ret);
}

static ERL_NIF_TERM
rawfd_pread(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int fd, size, pos;
    ssize_t num_read;
    ErlNifBinary bin;
    ERL_NIF_TERM ret;

    if(!enif_get_int(env, argv[0], &fd)) {
        fprintf(stderr, "Invalid fd.\r\n");
        return enif_make_badarg(env);
    }

    if(!enif_get_int(env, argv[1], &pos)) {
        fprintf(stderr, "Invalid offset.\r\n");
        return enif_make_badarg(env);
    }

    if(!enif_get_int(env, argv[2], &size)) {
        fprintf(stderr, "Invalid size.\r\n");
        return enif_make_badarg(env);
    }

    if(!enif_alloc_binary(size, &bin)) {
        fprintf(stderr, "Error allocating binary.\r\n");
        return enif_make_badarg(env);
    }

    num_read = pread(fd, bin.data, bin.size, pos);
    if(num_read < 0) {
        fprintf(stderr, "Error reading data.\r\n");
        goto error;
    } else if(num_read < bin.size) {
        if(!enif_realloc_binary(&bin, num_read)) {
            fprintf(stderr, "Error reallocating binary.\r\n");
            goto error;
        }
    }

    ret = enif_make_binary(env, &bin);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), ret);

error:
    enif_release_binary(&bin);
    return enif_make_badarg(env);
}

static ERL_NIF_TERM
rawfd_pwrite(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    int fd, pos;
    ssize_t num_written, total;
    ErlNifBinary bin;

    if(!enif_get_int(env, argv[0], &fd)) {
        fprintf(stderr, "Invalid fd.\r\n");
        return enif_make_badarg(env);
    }

    if(!enif_get_int(env, argv[1], &pos)) {
        fprintf(stderr, "Invalid position.\r\n");
        return enif_make_badarg(env);
    }

    if(!enif_inspect_binary(env, argv[2], &bin)) {
        fprintf(stderr, "Invalid binary data.\r\n");
        return enif_make_badarg(env);
    }

    total = 0;
    while(total < bin.size) {
        num_written = pwrite(fd, bin.data+total, bin.size-total, pos+total);
        if(num_written < 0) {
            perror("Error writing data:");
            fprintf(stderr, "Error writing data.\r\n");
            return enif_make_badarg(env);
        } else if(num_written == 0) {
            fprintf(stderr, "No data written.\r\n");
            return enif_make_badarg(env);
        }
        total += num_written;
    }

    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_funcs[] =
{
    {"open", 2, rawfd_open},
    {"pread", 3, rawfd_pread},
    {"pwrite", 3, rawfd_pwrite}
};

ERL_NIF_INIT(rawfd, nif_funcs, NULL, NULL, NULL, NULL);
