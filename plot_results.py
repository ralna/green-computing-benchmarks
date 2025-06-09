import matplotlib.pyplot as plt
import csv
import glob

csv_files = glob.glob("*.csv")

for c in csv_files:
    ax = plt.axes()
    print(c)
    with open(c) as file:
        reader = csv.DictReader(file)
        
        for row in reader:
        
            blas_backend = row['BLAS backend']
            gflops = {int(k.strip()): float(v.strip()) for k, v in row.items() if k != "BLAS backend" and k != ""}
        
            ax.semilogx(gflops.keys(), gflops.values(), label=blas_backend)
        
    ax.set_xticks(list(gflops.keys()))
    ax.set_xticklabels(gflops.keys())
    
    ax.set_xlabel("Problem size")
    ax.set_ylabel("GFLOP/s")
    ax.legend()
    ax.grid()    
            
    plt.savefig(c.split(".")[0] + ".png")
    plt.clf()