class Variante_anotacion:
    
    def __init__(self, _id):
        self._id = _id
        self.impact = ""
        self.clin_sig = ""
        self.symbol = ""
        self.existing_variant = ""
        self.sift = ""
        
    def __repr__(self):
        return "Variant:" + self._id
    
    def __str__(self):
        return "Variant:" + self._id