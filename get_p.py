import math
def chi2_cdf(x, k):
    # For even k, we can do it exactly. For odd, we need gamma, but k=9 is odd.
    # Let's just use a simple approximation or standard library
    import scipy.stats as st
    return 1 - st.chi2.cdf(x, k)
try:
    import scipy.stats as st
    print(1 - st.chi2.cdf(20.88, 9))
except:
    print("no scipy")
