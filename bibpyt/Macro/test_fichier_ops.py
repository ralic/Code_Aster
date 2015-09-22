# coding=utf-8
# ======================================================================
# COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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

import sys
import os
import re

# hashlib only exists in python>=2.5


def hash_new():
    try:
        import hashlib
        _hash_new = hashlib.md5()
    except ImportError:
        import md5
        _hash_new = md5.new()
    return _hash_new


class TestFichierError(Exception):
    pass


def convert(x):
    return float(x)


def f_SOMM(somme, lx):
    return somme + sum([convert(x) for x in lx])


def f_SOMM_ABS(somme, lx):
    return somme + sum([abs(convert(x)) for x in lx])


def f_MINI(val, lx):
    return min(val, min([convert(x) for x in lx]))


def f_MAXI(val, lx):
    return max(val, max([convert(x) for x in lx]))


def f_MINI_ABS(val, lx):
    return min(val, min([abs(convert(x)) for x in lx]))


def f_MAXI_ABS(val, lx):
    return max(val, max([abs(convert(x)) for x in lx]))

dict_func_test = {
    'SOMM': f_SOMM,
    'SOMM_ABS': f_SOMM_ABS,
    'MINI': f_MINI,
    'MAXI': f_MAXI,
    'MINI_ABS': f_MINI_ABS,
    'MAXI_ABS': f_MAXI_ABS,
}

#-------------------------------------------------------------------------


def test_fichier_ops(self, FICHIER, NB_VALE, NB_VALE_I,
                     VALE_CALC, VALE_CALC_I, VALE_CALC_K, TYPE_TEST,
                     TOLE_MACHINE, CRITERE, INFO, **kwargs):
    """
      Macro permettant de tester la non-regression d'un fichier.
      On teste le nombre de réels présents, et, facultativement, la
      somme de ces nombres et le texte du fichier.
    """
    import aster
    from Accas import _F
    from Utilitai.Utmess import UTMESS
    ier = 0
    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)
    # On importe les definitions des commandes a utiliser dans la macro
    # Le nom de la variable doit etre obligatoirement le nom de la commande
    INFO_EXEC_ASTER = self.get_cmd('INFO_EXEC_ASTER')
    DETRUIRE = self.get_cmd('DETRUIRE')
    CREA_TABLE = self.get_cmd('CREA_TABLE')
    TEST_TABLE = self.get_cmd('TEST_TABLE')
    #
    is_ok = 0
    TYPE_TEST = TYPE_TEST or 'SOMM'
    # # vérification non faisable dans le catalogue
    if VALE_CALC_I is not None and NB_VALE_I is None:
        UTMESS('F', 'TEST0_5')
    # vérifier que le fichier a été fermé
    __tinfo = INFO_EXEC_ASTER(LISTE_INFO='ETAT_UNITE', FICHIER=FICHIER)
    if __tinfo['ETAT_UNITE', 1].find('OUVERT') > -1:
        UTMESS('S', 'TEST0_2', valk=FICHIER)
    # lecture du fichier
    if not os.path.isfile(FICHIER):
        UTMESS('S', 'TEST0_3', valk=FICHIER)
    fileobj = open(FICHIER, 'r')
    # filtre par expression régulière
    try:
        fileobj = regexp_filter(fileobj, kwargs['EXPR_IGNORE'])
    except TestFichierError, valk:
        UTMESS('S', 'TEST0_1', valk=valk)
    # calcule le nombre de valeurs et la somme ou min/max
    verbose = INFO > 1
    results = test_iter(fileobj, function=dict_func_test[TYPE_TEST],
                        verbose=verbose)
    fileobj.close()
    nbvalr, vale_r, nbvali, vale_i, chksum = results
    # produit le TEST_TABLE
    refsum = VALE_CALC_K or 'not_tested'
    is_ok = int(chksum == refsum)
    mcfact = [
        _F(PARA='NBVAL_I', LISTE_I=nbvali),
        _F(PARA='VALE_I', LISTE_I=vale_i),
        _F(PARA='NBVAL', LISTE_I=nbvalr),
        _F(PARA='VALE', LISTE_R=vale_r),
        _F(PARA='TEXTE', LISTE_I=is_ok),
    ]
    __tab1 = CREA_TABLE(LISTE=mcfact)
    # message
    UTMESS('I', 'TEST0_4', valk=FICHIER)
    UTMESS('I', 'TEST0_13')
    if verbose or NB_VALE_I is not None:
        UTMESS('I', 'TEST0_15', vali=(nbvali, NB_VALE_I or 0))
    if verbose or VALE_CALC_I is not None:
        UTMESS('I', 'TEST0_16', vali=(vale_i, VALE_CALC_I or 0))
    UTMESS('I', 'TEST0_14')
    UTMESS('I', 'TEST0_15', vali=(nbvalr, NB_VALE or 0))
    if verbose or VALE_CALC is not None:
        UTMESS('I', 'TEST0_17', valr=(vale_r, VALE_CALC or 0.))
    if verbose or VALE_CALC_K is not None:
        UTMESS('I', 'TEST0_18', valk=(chksum, refsum))
    # tests
    TEST_TABLE(TABLE=__tab1,
               NOM_PARA='NBVAL',
               VALE_CALC_I=NB_VALE,
               CRITERE='ABSOLU',
               TOLE_MACHINE=0,)
    if VALE_CALC is not None:
        TEST_TABLE(TABLE=__tab1,
                   NOM_PARA='VALE',
                   CRITERE=CRITERE,
                   VALE_CALC=VALE_CALC,
                   TOLE_MACHINE=TOLE_MACHINE,)
    if NB_VALE_I is not None:
        TEST_TABLE(TABLE=__tab1,
                   NOM_PARA='NBVAL_I',
                   VALE_CALC_I=NB_VALE_I,
                   CRITERE='ABSOLU',
                   TOLE_MACHINE=0,)
    if VALE_CALC_I is not None:
        TEST_TABLE(TABLE=__tab1,
                   NOM_PARA='VALE_I',
                   CRITERE=CRITERE,
                   VALE_CALC_I=VALE_CALC_I,
                   TOLE_MACHINE=TOLE_MACHINE,)
    if VALE_CALC_K is not None:
        TEST_TABLE(TABLE=__tab1,
                   NOM_PARA='TEXTE',
                   VALE_CALC_I=int(True),
                   TOLE_MACHINE=0,
                   CRITERE='ABSOLU',)
    return ier


def regexp_filter(file_in, regexp_ignore, debug=False):
    """Filtre le fichier fourni (file descriptor) en utilisant les
    expressions régulières fournies.
    On retourne l'objet file vers le fichier modifié (ou non).
    """
    if not regexp_ignore:      # None or []
        return file_in
    # vérification des expressions régulières
    if type(regexp_ignore) not in (list, tuple):
        regexp_ignore = [regexp_ignore, ]
    l_regexp = []
    for exp in regexp_ignore:
        try:
            obj = re.compile(exp)
        except re.error, s:
            raise TestFichierError, (s, str(exp))
        else:
            l_regexp.append(obj)
    # filtre du fichier
    file_out = os.tmpfile()
    file_in.seek(0)
    for i, line in enumerate(file_in):
        if debug:
            print 'LIGNE', i,
        keep = True
        for exp in l_regexp:
            if exp.search(line):
                keep = False
                if debug:
                    print ' >>>>>>>>>> IGNOREE <<<<<<<<<<'
                break
        if keep:
            file_out.write(line)
            if debug:
                print
    file_out.seek(0)
    return file_out


RE_FLOAT_EXPO = re.compile('[-+]?[0-9\.]+[eED][\-\+]{0,1}[0-9]+')
RE_FLOAT = re.compile('[-+]?[0-9]+?\.[0-9]*')
RE_INT = re.compile('[0-9]+')

def test_iter(obj, function, verbose=False):
    """
    Cette fonction compte le nombre de réels dans le fichier et une grandeur
    à partir des valeurs (somme, sommes des valeurs absolues, min/max...).
    IN :
       obj      : objet 'file' ou 'string' sur le lequel on peut itérer
       function : fonction de test   val = func_test(val, [xi, ...])
       verbose  : on affiche le résumé si info>0
    OUT :
       nombre de valeurs réelles, résultat de la fonction sur les réels,
       nombre de valeurs entières, résultat de la fonction sur les entiers,
       somme de contrôle du texte restant.
    Le résultat entier est systématiquement retourné modulo 2147483647
    pour être homogène avec les plate-formes 32 bits.
    Le nombre de valeurs lui est supposé être inférieur à cette valeur.
    """
    max_buff_size = 1000
    nbvalr = 0
    nbvali = 0
    valr = 0.
    vali = 0.
    hfile = hash_new()
    # Si on lit tout le fichier d'un coup, on va environ 3 fois plus vite
    # que si on le lit ligne à ligne, mais on consomme en mémoire environ
    # 5 fois la taille du fichier...
    # En lisant par paquet de 1000 (ou 10000), on va quasiment aussi vite
    # en consommant très peu de mémoire.
    #    fichier     tout   ligne/ligne   1000 lignes
    #     10 Mo       3 s      10 s       3 s
    #     50 Mo      17 s      48 s      17 s
    #    100 Mo      34 s      96 s      35 s
    # l'itérateur est l'objet file lui-même ou on le crée sur la liste
    if type(obj) is file:
        obj.seek(0)
        iterator = obj
    else:
        iterator = iter(obj)
    ok = True
    buff = []
    while ok:
        try:
            text = iterator.next()
        except StopIteration:
            ok = False
            text = ''
        buff.append(text)
        if ok and len(buff) < max_buff_size:
            continue
        else:
            text = ''.join(buff)
            buff = []
        # extract floats
        l_float = RE_FLOAT_EXPO.findall(text)
        l_float = [s.replace('D', 'E') for s in l_float]
        text = RE_FLOAT_EXPO.sub('', text)
        l_float.extend(RE_FLOAT.findall(text))
        text = RE_FLOAT.sub('', text)
        nbvalr += len(l_float)
        valr = function(valr, l_float)
        # extract integers
        l_int = RE_INT.findall(text)
        text = RE_INT.sub('', text)
        nbvali += len(l_int)
        vali = function(vali, l_int)
        # add text
        text = ''.join([s.strip() for s in text.split()])
        hfile.update(text)
        if verbose:
            print 'Nombres réels :', nbvalr
            print l_float
            print 'Nombres entiers :', nbvali
            print l_int
            print 'Texte :'
            print text
    chksum = hfile.hexdigest()
    return nbvalr, valr, nbvali, int(vali) % 2147483647, chksum


def test_file(filename, regexp_ignore=[], type_test='SOMM', verbose=False):
    """Raccourci pour tester rapidement un fichier (utilisé par stanley.py).
    """
    if type(regexp_ignore) not in (list, tuple):
        regexp_ignore = [regexp_ignore, ]

    fileobj = open(filename, 'r')
    fileobj = regexp_filter(fileobj, regexp_ignore)

    return test_iter(
        fileobj, function=dict_func_test[type_test], verbose=verbose)


if __name__ == '__main__':
    from optparse import OptionParser, OptionGroup

    p = OptionParser(usage='usage: %s fichier [options]' % sys.argv[0])
    p.add_option('--type_test',
                 action='store', dest='type_test', default='SOMM',
                 help='type du test : SOMM, SOMM_ABS, MIN, MAX')
    p.add_option('--expr_ignore',
                 action='store', dest='exp', type='string',
                 help='expression régulière à ignorer')
    p.add_option('-v', '--verbose',
                 action='store_true', dest='verbose', default=False,
                 help='mode bavard')
    opts, args = p.parse_args()

    if len(args) == 0:
        p.error('fichier à tester ?')

    if opts.exp is None:
        exp = []
    else:
        exp = [opts.exp]

    fileobj = open(args[0], 'r')
    fileobj = regexp_filter(fileobj, exp)
    results = test_iter(fileobj, function=dict_func_test[opts.type_test],
                        verbose=opts.verbose)
    print '%6d réels, vale_r = %f, %6d entiers, vale_i = %d, texte : %s' % results
