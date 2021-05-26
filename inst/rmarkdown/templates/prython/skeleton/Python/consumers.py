import pandas as pd
from numpy.random import default_rng

def qd_generator(alpha, beta):
    def qd(p):
        return max(0, alpha-beta*p)
    return qd

class Consumer:
    
    consumers500 = []
    
    def __init__(self, alpha, beta):
        self.alpha=alpha
        self.beta=beta
        self.qd = qd_generator(alpha, beta)
        Consumer.consumers500.append(self)
        # some prefer self.__class__.consumers500.append(self)
    
    @classmethod # notice the declaration of classmethod
    
    def Qd(cls, p): # add cls to its first input
        total_q = sum([cls.consumers500[i].qd(p) for i in range(len(cls.consumers500))])
        return total_q


def pd_qdsGenerator(p):
    qds_at_p = [[i, p, consumer_i.qd(p)] for i, consumer_i in enumerate(consumers500)]
    pd_qds = pd.DataFrame(qds_at_p, columns = ["ID","price","quantity"])
    return pd_qds
