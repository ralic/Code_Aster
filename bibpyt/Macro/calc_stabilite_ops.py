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
import aster
from Utilitai.Table import Table
from numpy import *
from numpy.linalg import *
from Calc_essai.cata_ce import crea_champ
from Utilitai.Utmess import UTMESS

def calc_stabilite_ops(self, reuse, SCHEMA_TEMPS, FILTRE, **args):
    """
    Filtre sur une table mode_non_line.
    On evalue egalement la stabilite du systeme
    """

    from Accas import _F

    # La macro compte pour 1 dans la numerotation des commandes
    self.set_icmd(1)

    # Le concept sortant est une table_container
    self.DeclareOut('t_resu', self.sd)

    # On importe les definitions des commandes a utiliser dans la macro
    EXTR_TABLE = self.get_cmd('EXTR_TABLE')

    t_mnl = args['MODE_NON_LINE'].EXTR_TABLE()
    nbord = len(t_mnl.rows)

    rows_tab = []
    t_res = Table(rows=rows_tab,
                        para=t_mnl.para,
                        typ= t_mnl.type,
                        titr='Table extraite')

    if FILTRE:
        if FILTRE[0]['NUME_ORDRE']:
            l_ordre = FILTRE[0]['NUME_ORDRE']
        else:
            fmin = FILTRE[0]['FREQ_MIN']
            fmax = FILTRE[0]['FREQ_MAX']
            prec = FILTRE[0]['PRECISION']
            l_ordre = []
            for i in range(nbord):
                frequence = t_mnl['FREQUENCE'].values()['FREQUENCE'][i]
                if frequence > fmin-prec and frequence < fmax+prec:
                    l_ordre.append(
                        t_mnl['NUME_ORDRE'].values()['NUME_ORDRE'][i])
    else:
        l_ordre = t_mnl['NUME_ORDRE'].values()['NUME_ORDRE']

    recup_para = 0
    for num_ordr in l_ordre:

        filtre = {}
        filtre['FILTRE'] = _F(NOM_PARA= 'NUME_ORDRE',
                          VALE_I= num_ordr)

        __sol_per = EXTR_TABLE(TABLE= args['MODE_NON_LINE'],
                        TYPE_RESU= 'MODE_MECA',
                        NOM_PARA= 'NOM_SD',
                        **filtre)

        if not recup_para:
            iret, ibid, kass_name = aster.dismoi(
                'REF_RIGI_PREM', __sol_per.nom, 'RESU_DYNA', 'F')
            iret, ibid, mass_name = aster.dismoi(
                'REF_MASS_PREM', __sol_per.nom, 'RESU_DYNA', 'F')

            ctx = CONTEXT.get_current_step().get_contexte_courant()
            kass = ctx[kass_name]
            masse = ctx[mass_name]

            __choc = EXTR_TABLE(TABLE=args['MODE_NON_LINE'],
                     TYPE_RESU= 'TABLE_SDASTER',
                     NOM_PARA= 'CARA_CHOC',
                        **filtre)

            t_choc = __choc.EXTR_TABLE()

            typ = t_choc['TYPE_CHOC'].values()['TYPE_CHOC']
            alpha = t_choc['RIGI_NOR'].values()['RIGI_NOR']
            eta = t_choc['PARA_REGUL'].values()['PARA_REGUL']
            jeu = t_choc['JEU'].values()['JEU']
            noeud1 = t_choc['NOEUD_CHOC'].values()['NOEUD_CHOC']
            ncmp1 = t_choc['NOM_CMP_1'].values()['NOM_CMP_1']
            ncmp2 = t_choc['NOM_CMP_2'].values()['NOM_CMP_2']
            orig1 = t_choc['ORIG_OBST_X'].values()['ORIG_OBST_X']
            orig2 = t_choc['ORIG_OBST_Y'].values()['ORIG_OBST_Y']
            orig3 = t_choc['ORIG_OBST_Z'].values()['ORIG_OBST_Z']

            nchoc = len(t_choc.rows)
            recup_para = 1

        t_freq = t_mnl['FREQUENCE'].values()['FREQUENCE']
        t_ordre = t_mnl['NUME_ORDRE'].values()['NUME_ORDRE']
        t_modes = t_mnl['NB_COEF_FOURIER'].values()['NB_COEF_FOURIER']
        for ilig in range(len(t_ordre)):
            if t_ordre[ilig] == num_ordr:
                freq = t_freq[ilig]
                nbmod = t_modes[ilig]

        hu = int((nbmod-1)/2)
        omega = freq*(2*pi)
        [pos1r, ind1, rig] = extr_matr(kass)
        [pos2r, ind2, mass] = extr_matr(masse)

#
# ELIMINATION DES CONDITIONS AUX LIMITES DES COEFFICIENTS DE FOURIER
#
        u_extend = []
        for k in range(1, nbmod + 1):
            champ = crea_champ(__sol_per, k)
            u_extend.append(champ)
        u_extend = transpose(array(u_extend))

        u1 = zeros((len(rig), nbmod))
        j = 0
        for k in range(len(ind1)):
            if(ind1[k] == 1):
                u1[j,:] = u_extend[k,:]
                j = j+1
        u = u1/max(jeu)

#
# RECUPERATION DES INDICES OU SE TROUVENT LES DDLS DE CHOCS
#
        poschoc = posnoeud(typ, noeud1, ncmp1, ncmp2, pos1r)
        orig = zeros(3*nchoc)
        for k in range(nchoc):
            orig[3*k] = orig1[k]
            orig[3*k+1] = orig2[k]
            orig[3*k+2] = orig3[k]

        # adimensionnement
        nd = int(size(u)/(2*hu+1))
        vect1 = zeros(nd)
        vect1[poschoc[0]-1] = 1.E+00
        adim = zeros(3)
        rig1 = rig#/adim[0]
        mass1 = mass#/adim[1]
        alpha1 = alpha#/adim[0]
        omega1 = omega#/adim[2]
        jeu1 = zeros(nchoc)
        for k in range(nchoc):
            jeu1[k] = jeu[k]/max(jeu)

        nbpas = SCHEMA_TEMPS[0]['NB_INST']
        eps = args['TOLERANCE']
        info = args['INFO']

        UTMESS('I', 'MECANONLINE9_67', vali= num_ordr)
        stab = main(u, hu, omega1, rig1, mass1, nchoc, poschoc, orig, typ, alpha1, eta, jeu1, nbpas, eps, info)

        i = t_mnl['NUME_ORDRE'].values()['NUME_ORDRE'].index(num_ordr)
        if(stab):
            t_mnl.rows[i]['STABILITE'] = 'STABLE'
        else:
            t_mnl.rows[i]['STABILITE'] = 'INSTABLE'
        t_res.rows.append(t_mnl.rows[i])

    if self.reuse:
# surcharge de la table
        t_res.rows = []
        for i in range(nbord):
            t_res.rows.append(t_mnl.rows[i])

        DETRUIRE = self.get_cmd('DETRUIRE')
        DETRUIRE(CONCEPT=_F(NOM=args['MODE_NON_LINE']), INFO=1)

    CREA_TABLE = self.get_cmd('CREA_TABLE')
    tab = t_res.dict_CREA_TABLE()
    t_resu = CREA_TABLE(TYPE_TABLE='TABLE_CONTENEUR', **tab)

    return


def main(U, hu, omega, K, M, nchoc, poschoc, orig, typchoc, alpha, eta, jeu, nbinst, eps, info):
    # Taille du vecteur
    N = size(U)
    # Nombre de ddls actifs
    nd = int(N/(2*hu+1))

    # On definit la matrice de monodromie
    Mmono = zeros((2*nd, 2*nd))

    # Paramètres Newmark
    alp = 0.5
    bet = 0.25
    dt = 2 * pi/(omega*(nbinst-1))

    # Initialisation
    dFdU = zeros((nd, nd))
    phi = zeros((nd, 2*nd))
    dphi = zeros((nd, 2*nd))
    ddphi = zeros((nd, 2*nd))

    a1 = (1.-alp)*dt
    b1 = (0.5-bet)*dt**2
    a2 = 1./(bet*(dt**2))
    b2 = alp/(bet*dt)

    t = 0.
    phi[:, :nd] = eye(nd, nd)
    dphi[:, nd:2 * nd] = eye(nd, nd)
    Keff = K + a2 * M
    dFdU = dfdu(t, nd, hu, U, omega, nchoc, poschoc, orig, typchoc, alpha, eta, jeu)
    ddphi = -solve(M, dot(K+dFdU, phi))

    for kt in range(nbinst - 1):
        phi = phi+dt*dphi+b1*ddphi
        dphi = dphi+a1*ddphi

        dFdU = dfdu(t+dt, nd, hu, U, omega, nchoc, poschoc, orig, typchoc, alpha, eta, jeu)
        S = (Keff+dFdU)/a2
        ddphi = -solve(S, dot(K+dFdU, phi))

        phi = phi+ddphi/a2
        dphi = dphi+alp*dt*ddphi

        nrma = zeros(2*nd)
        nrm = zeros(2*nd)
        for k in range(2 * nd):
            nrma[k] = norm(dot(M, ddphi[:, k])+dot(K, phi[:, k])+dot(dFdU, phi[:, k]))
            nrm[k] = nrma[k]/norm(phi[:, k])

        nrmaT = sum(nrma)/(2*nd)
        nrmT = sum(nrm)/(2*nd)
        t = t+dt

    # UTMESS('I', 'MECANONLINE9_64', valr = (nrmaT,nrmT)
    # UTMESS('I', 'MECANONLINE9_65', valr = nrmT)

    Mmono[0:nd,:] = phi
    Mmono[nd:2*nd,:] = dphi

    # Calcul vecteur et valeur propre
    [valp, vectp] = eig(Mmono)

    # UTMESS('I', 'MECANONLINE9_66', valr = max(abs(valp)))
    stable = prod(abs(valp) < 1+eps, dtype='bool')

    if(stable):
        UTMESS('I', 'MECANONLINE9_65')
    else:
        UTMESS('I', 'MECANONLINE9_66')

    if(info == 2):
        UTMESS('I', 'MECANONLINE9_64', valr= (nrmaT, nrmT, max(abs(valp))))

    return stable


def dfdu(t, nd, hu, U, omega, nchoc, poschoc, orig, typchoc, alpha, eta, jeu):
    lig = ones(nchoc*6)
    col = ones(nchoc*6)
    val = zeros(nchoc*6)

    dFdU = zeros((nd, nd))
    for k in range(nchoc):
        if(typchoc[k].strip() == 'CERCLE'):
            px = poschoc[k*6]-1
            py = poschoc[k*6+1]-1
            ox = orig[k*3]
            oy = orig[k*3+1]

            ux = (ut(U, t, omega, hu, nd, px)-ox)/jeu[k]
            uy = (ut(U, t, omega, hu, nd, py)-oy)/jeu[k]

            ur = sqrt(ux**2+uy**2)
            fn = funilateral(ur, alpha[k], eta[k])
            dfndr = fn/((2/alpha[k])*fn-(ur-1))

            drdux = ux/ur
            drduy = uy/ur

            dfxdux = dfndr*drdux*ux/ur+fn*(ur-drdux*ux)/(ur**2)
            dfxduy = dfndr*drduy*ux/ur+fn*(-drduy*ux)/(ur**2)

            dfyduy = dfndr*drduy*uy/ur + fn*(ur-drduy*uy)/(ur**2)
            dfydux = dfndr*drdux*uy/ur + fn*(-drdux*uy)/(ur**2)

            dFdU[px, px] = dFdU[px, px]+jeu[k]*dfxdux
            dFdU[px, py] = dFdU[px, py]+jeu[k]*dfxduy
            dFdU[py, px] = dFdU[py, px]+jeu[k]*dfydux
            dFdU[py, py] = dFdU[py, py]+jeu[k]*dfyduy

        elif(typchoc[k].strip() == 'BI_PLAN'):
            pos = poschoc[k*6]-1
            x = ut(U, t, omega, hu, nd, pos)/jeu[k]
            f = fbilateral(x, alpha[k], eta[k])
            val = jeu[k]*(2.*(f**2)/alpha[k]-2.*x*f-eta[k])/(3.*(f/alpha[k])**2-4.*x*f/alpha[k]+(x**2-1.))
            dFdU[pos, pos] = dFdU[pos, pos]+val

        elif(typchoc[k].strip() == 'PLAN'):
            pos = poschoc[k*6]-1
            x = ut(U, t, omega, hu, nd, pos)/jeu[k]
            fn = funilateral(x, alpha[k], eta[k])
            val = jeu[k]*(fn/((2/alpha[k])*fn-(x-1)))
            dFdU[pos, pos] = dFdU[pos, pos]+val

    return dFdU


def funilateral(x, alpha, eta):
    fn = ((x-1.)+sqrt((x-1.)**2+4.*eta/alpha))/(2./alpha)
    return fn


def fbilateral(x, alpha, eta):
    a1 = 1./(alpha**2)
    b1 = -2.*x/alpha
    c1 = (x**2-1)
    d1 = eta*x

    h = -b1/(3.*a1)

    p = (3.*a1*h**2+2.*b1*h+c1)/a1
    q = -(a1*h**3+b1*h**2+c1*h+d1)/a1

    discr1 = q**2 + (4.*p**3)/27.

    sd = (1-sign(discr1))/2

    u3 = (q - (1j**sd)*sqrt(abs(discr1)))/2.
    v3 = (q + (1j**sd)*sqrt(abs(discr1)))/2.

    u = u3**(1/3.);
    v = v3**(1/3.);

    f1 = u + v + h

    a2 = a1
    b2 = b1+a1*f1
    c2 = c1+b1*f1 + a1*f1**2

    discr2 = b2 ** 2 - 4.*a2*c2

    f = (-b2 + sqrt(b2 ** 2 - 4.*a2*c2))/(2.*a2)

    return f.real


def ut(U, t, omega, hu, nd, pos):
    kk = linspace(1., hu, hu)
    vcos = cos(omega*t*kk)
    vsin = sin(omega*t*kk)
    Ut = U[pos, 0]+dot(U[pos, 1:hu+1], vcos)+dot(U[pos, hu+1:2*hu+1], vsin)
    return Ut


def posnoeud(typ, noeud1, comp1, comp2, pos):
    n = len(noeud1)
    nddl = len(pos)

    ncmp = ['']*6

    poschoc = zeros(6*n, dtype='int')
    for k in range(n):
        if(typ[k].strip() == 'CERCLE'):
            ncmp[0] = comp1[k]
            ncmp[1] = comp2[k]
            for j in range(nddl):
                if(noeud1[k].strip()+ncmp[0].strip() == pos[j]):
                    poschoc[k*6] = j+1
                elif(noeud1[k].strip()+ncmp[1].strip() == pos[j]):
                    poschoc[k*6+1] = j+1
        elif(typ[k].strip() == 'BI_PLAN'):
            ncmp[0] = comp1[k]
            for j in range(nddl):
                if(noeud1[k].strip()+ncmp[0].strip() == pos[j]):
                    poschoc[k*6] = j+1
        elif(typ[k].strip() == 'PLAN'):
            ncmp[0] = comp1[k]
            for j in range(nddl):
                if(noeud1[k].strip()+ncmp[0].strip() == pos[j]):
                    poschoc[k*6] = j+1
    return poschoc


def extr_matr(matr):
    nommatr = str(matr.nom).strip()
    lenm = len(nommatr)
    nommatr = nommatr+' '*(8-lenm)
    nrefa = nommatr+'           .REFA'
    vrefa = aster.getvectjev(nrefa)
    numeddl = str(vrefa[1]).strip()
    lenn = len(numeddl)
    numeddl = numeddl+' '*(14-lenn)
    typm = str(vrefa[8]).strip()

    # on recupere les valeurs de la matrice
    nvalm = nommatr+'           .VALM'
    vvalm = aster.getcolljev(nvalm)
    # print(vvalm[1])

    nsmdi = numeddl+'.SMOS.SMDI'
    nsmhc = numeddl+'.SMOS.SMHC'
    vsmdi = aster.getvectjev(nsmdi)
    vsmhc = aster.getvectjev(nsmhc)

    valm1 = vvalm[1]
    if(typm == 'MS'):
# Si la matrice est symétrique
        valm2 = vvalm[1]
    else:
# Si la matrice est non-symétrique
        valm2 = vvalm[2]

# Taille de la matrice avec cdl
    neq = len(vsmdi)
    # print(neq)

# Nombre de termes non-nuls
    nterm = len(vsmhc)
    # print(nterm)

# Creation de la matrice AVEC cdl
    matriceg = zeros([neq, neq])
    j = 0
    for k in range(nterm):
        matriceg[vsmhc[k]-1][j] = valm1[k]
        matriceg[j][vsmhc[k]-1] = valm2[k]
        if((vsmdi[j]-1) == k):
            j = j+1

    # print(matriceg)

# Création du vecteur ind : 1 si ddl phys 0 sinon
# Cas CDL = AFFE_CHAR_CINE
    nccid = nommatr+'           .CCID'
    vccid = aster.getvectjev(nccid)
    # print(vccid)

# Cas CDL = AFFE_CHAR_MECA
    ndeeq = numeddl+'.NUME.DEEQ'
    vdeeq = aster.getvectjev(ndeeq)
    # print('vdeeq')
    # print(vdeeq)

    ind = zeros(neq)
# Cas CDL = AFFE_CHAR_CINE
    if(vccid is not None):
        ind = ones(neq)-vccid[0:neq]
        # print(ind)
        nd = neq-vccid[neq]
        # print(nd)
# Cas CDL = AFFE_CHAR_MECA
    elif(vdeeq is not None):
        for k in range(neq):
            if(vdeeq[-1+2*k+2] > 0):
                ind[k] = 1
            elif(vdeeq[-1+2*k+2] < 0):
                ind[k] = 0
                j = 0
                tcmp = -vdeeq[-1+2*k+2]
                while((vdeeq[-1+2*j+1] != vdeeq[-1+2*k+1]) or (vdeeq[-1+2*j+2] != tcmp)):
                    j = j+1
                ind[j] = 0
        # print(ind)
        nd = 0
        for k in range(neq):
            if(ind[k] == 1):
                nd = nd+1

# Creation de la matrice SANS cdl
    matrice = zeros([nd, nd])
    ii = 0
    for i in range(neq):
        jj = 0
        if(ind[i] == 1):
            for j in range(neq):
                if(ind[j] == 1):
                    # print(i,ind[i],ii)
                    # print(j,ind[j],jj)
                    matrice[ii][jj] = matriceg[i][j]
                    jj = jj+1
            ii = ii+1
    # print(matrice)

# Creation du vecteur [N1DX ... N?D?] de taille nd
    vectddl = ['']*nd
    comp = ['DX', 'DY', 'DZ', 'DRX', 'DRY', 'DRZ',]
    i = 0
    for k in range(neq):
        if(ind[k] == 1):
            vectddl[i] = 'N'+str(vdeeq[2*k]).strip()+comp[vdeeq[2*k+1]-1].strip()
            i = i+1
    if(vccid is not None):
        inds = ind
    else:
        inds = zeros(len(ind))
        i = 0
        for k in range(neq):
            if(vdeeq[2*k+1] > 0):
                inds[i] = ind[i]
                i = i+1
        inds = inds[0:i]
    return [vectddl, inds, matrice]
