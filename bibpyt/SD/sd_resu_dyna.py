# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
# THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
# IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
# THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
# (AT YOUR OPTION) ANY LATER VERSION.
#
# THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
# WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
# MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
# GENERAL PUBLIC LICENSE FOR MORE DETAILS.
#
# YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
# ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
#    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
# ======================================================================
# person_in_charge: hassan.berro at edf.fr

from SD import *

from SD.sd_nume_ddl         import sd_nume_ddl
from SD.sd_prof_chno        import sd_prof_chno
from SD.sd_nume_ddl_gene    import sd_nume_ddl_gene
from SD.sd_matr_asse        import sd_matr_asse
from SD.sd_matr_asse_gene   import sd_matr_asse_gene
from SD.sd_interf_dyna_clas import sd_interf_dyna_clas
from SD.sd_proj_mesu        import sd_proj_mesu
from SD.sd_resultat         import sd_resultat

class sd_resu_dyna(AsBase):
#--------------------------
    nomj = SDNom(fin=19)

    # Indirecton vector, relating the index of each saved field to a valid REFD entry 
    INDI = AsVI  (SDNom(debut=19),)
    # Collection where different types of references are saved
    REFD = AsColl(SDNom(debut=19),acces='NU',stockage='CONTIG',type='K', ltyp=24,)
    
    # Optional data structure used for dynamic concepts produced by PROJ_MESU_MODAL
    PROJM = Facultatif(sd_proj_mesu(SDNom(debut=8)))

    def check_REFD(self,checker):
        REFDColl = self.REFD.get()
        # Convert into a simple python list by looping over the dictionnary keys
        REFDColl = [REFDColl[i] for i in REFDColl.keys()]
        # assert (True in [Val != None for Val in REFDColl])
        # Check each of the REFD entries, one by one
        for REFDEntry in REFDColl : CheckREFDEntry(self, REFDEntry, checker)

    def check_INDI(self,checker):
        # Check that the REFD collection contains as many elements as the indirection
        # object (INDI)
        NbEntriesREFD = self.REFD.nutioc
        NbEntriesINDI = sum([1 for i in self.INDI.get() if i != -100])
        assert NbEntriesREFD == NbEntriesINDI


def CheckREFDEntry(self, Entry, checker):
    if not(Entry) : return

    RefType = CheckAcceptableType(Entry, checker)

    # Some operators may create results with a nume_ddl but no information on the matrices
    # cf. PROJ_CHAMP, DEFI_BASE_MODALE, CALC_ESSAI, CREA_ELEM_SSD, and OBSERVATION
    if (RefType != 'DYNAMIQUE') : CheckNonEmptyEntry(Entry, checker)

    CoorType = CheckNumeDDL(self, Entry, checker)

    Checks   = {'DYNAMIQUE'  : [(3,CheckAssembledMat,{'Type':CoorType})],
                'INTERF_DYNA': [(1,CheckInterfDyna  ,{}),
                                (2,CheckEmpty       ,{} )],
                'INTERF_STAT': [(1,CheckInterfStat  ,{'Type':CoorType}),
                                (2,CheckEmpty       ,{} )],
                'MESURE'     : [(1,CheckModes       ,{'Type':CoorType}),
                                (2,CheckEmpty       ,{} )],                 } [RefType]
    ind = 2
    for Check in Checks :
        N, CheckFunc, Options = Check
        for i in range(ind,ind+N):
            CheckFunc(Entry[i].strip(), checker, **Options)
        ind += N

def CheckAcceptableType(Entry, checker):
    AccTypes    = ['DYNAMIQUE','INTERF_DYNA','INTERF_STAT','MESURE']
    RefType     = Entry[0].strip()
    assert (RefType in AccTypes)
    return RefType

def CheckNonEmptyEntry(Entry,checker):
    Entry = Entry[2:]
    Len = [len(Concept.strip()) for Concept in Entry]
    assert (sum(Len) != 0)

def CheckNumeDDL(self, Entry, checker):
    NumeName = Entry[1].strip()

    Type = 'PHYS'
    # The DISC object is only present in a dyna_gene type result.
    jvexist = aster.jeveux_exists
    if jvexist(self.nomj()[:19]+'.DISC') : Type = 'GENE'

    if not(NumeName) : return Type

    # In some cases, such as in CREA_RESU when no complete NUME_DDL information can be
    # found, the reference numbering corresponds to a sd_prof_chno
    ProfChNo = 0
    if len(NumeName)>8 : ProfChNo = 1

    NumeDDL = {'PHYS' : [sd_nume_ddl, sd_prof_chno],
               'GENE' : [sd_nume_ddl_gene]          }[Type][ProfChNo](NumeName)
    NumeDDL.check(checker)
    return Type

def CheckAssembledMat(MatName, checker, **args):
    if not(MatName) : return
    Type = args['Type']
    AsseMatrix = {'PHYS' : sd_matr_asse,
                  'GENE' : sd_matr_asse_gene}[Type](MatName)
    AsseMatrix.check(checker)

def CheckInterfDyna(InterfDName, checker, **args):
    assert (InterfDName)
    InterfD = sd_interf_dyna_clas(InterfDName)
    InterfD.check(checker)

def CheckResuDyna(ResuDynaName, checker, **args):
    # Used to check the mode_meca type concepts under INTERF_STAT and MESURES
    assert (ResuDynaName)
    ResuDyna = sd_resu_dyna(ResuDynaName)
    ResuDyna.check(checker)

def CheckResuPhys(ResuPhysName, checker, **args):
    assert (ResuPhysName)
    ResuPhys = sd_resultat(ResuPhysName)
    ResuPhys.check(checker)

#########################################################################################
### In the REFD, the only dyna_gene concept that can be found is a mode_gene, however
### it has to be checked as a normal sd_resultat (with CheckResuPhys), therefore there
### is no need for the following function, at least for the time being.
#########################################################################################
# def CheckResuGene(ResuGeneName, checker, **args):
#     assert (ResuGeneName)
#     ResuPhys = sd_dyna_gene(ResuGeneName)
#     ResuPhys.check(checker)

def CheckInterfStat(InterfSName, checker, **args):
    assert (InterfSName)
    Type = args['Type']
    if Type == 'PHYS':
        # Case of static modes of the types mode_meca and mult_elas
        CheckResuPhys(InterfSName, checker)
        # Case of static modes of the type mode_meca
        if IsResuDyna(InterfSName) : CheckResuDyna(InterfSName, checker)
    else : 
        print "A generalized numbering for a static interface reference result! Aborting."
        assert(False)

def CheckModes(ModesName, checker, **args):
    assert (ModesName)
    Type = args['Type']
    # TODO : when mode_gene becomes based on dyna_gene, we would update this line
    #        to use CheckResuGene instead ...
    CheckResuDyna(ModesName, checker)
    CheckResuPhys(ModesName, checker)

def CheckEmpty(Name, checker, **args):
    assert (len(Name) == 0)

def IsResuDyna(Name):
    jvexist = aster.jeveux_exists
    if jvexist(FillSp(Name,'.INDI')) and jvexist(FillSp(Name,'.REFD')) : 
        return True
    else :
        return False

def FillSp(St1, St2):
    NbSP = 24 - len(St1) - len(St2)
    return (St1+' '*NbSP+St2)