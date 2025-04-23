BLAS_BACKENDS=$(flexiblas list -p | cut -d '|' -f 2)

for B in ${BLAS_BACKENDS[@]};
do
    export FLEXIBLAS=$B
    ./build/green_computing_benchmark "${B}"
done