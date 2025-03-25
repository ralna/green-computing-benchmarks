BLAS_BACKENDS=("NETLIB" "OPENBLASPTHREAD" "MKLOPENMP" "MKLSERIAL")

for B in ${BLAS_BACKENDS[@]};
do
    export FLEXIBLAS=$B
    ./build/green_computing_benchmark "results_${B}.csv"
done