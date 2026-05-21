BLAS_BACKENDS=$(flexiblas list -p | cut -d '|' -f 2)
export OMP_NUM_THREADS=1

FILENAME=$1

for B in ${BLAS_BACKENDS[@]};
do
    export FLEXIBLAS=$B
    echo $B
    ./build/benchmark_blas $FILENAME
done