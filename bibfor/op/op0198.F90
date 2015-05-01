subroutine op0198()
    implicit   none
! ======================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
!
! ======================================================================
! ======================================================================
! --- BUT : COMMANDE POST_K_BETA ---------------------------------------
! ======================================================================
! ======================================================================
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/calck1.h"
#include "asterfort/coplas.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rechmc.h"
#include "asterfort/rechth.h"
#include "asterfort/recupe.h"
#include "asterfort/recutb.h"
#include "asterfort/recuvl.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/veritb.h"
#include "asterfort/wkvect.h"
#include "asterfort/calc_infl_k1.h"
    integer :: ndim, nk1d, ik1d, jnogn, itime, nbval, jtbint, nbval2
    integer :: norev, nomdb, ibid, nval
    real(kind=8) :: lrev, deklag, prodef, londef, temps, k1acp
    real(kind=8) :: lmdb
    real(kind=8) :: dkma, dkmb, dkmc, kal, kbl, kcl, k1a, k1b, k1c
    real(kind=8) :: tempa, tempb, k1bcp, k1ccp
    real(kind=8) :: rnom(10)
    real(kind=8) :: decal
    complex(kind=8) :: c16b
    character(len=8) :: result, k8b, noma, matrev, matmdb, oridef
    character(len=8) :: tabrev, tabmdb, tabthr, typpar(11)
    character(len=10) :: nomtab(11)
    character(len=12) :: profil
    character(len=16) :: nomcmd, nmgrno
    character(len=19) :: tbinst, tbscrv, tbscmb, sigmrv, sigmdb, tbinth
    character(len=32) :: knom
!
    data  nomtab / 'GROUP_NO',   'INST',     'K1_REV',  'KCP_REV',&
     &             'TEMPPF_REV', 'K1_MDB',   'KCP_MDB', 'TEMPPF_MDB',&
     &             'K1C_REV',    'KCPC_REV', 'TEMPFC_REV'/
    data  typpar / 'K32', 'R', 'R', 'R', 'R', 'R', 'R', 'R' ,'R' ,'R',&
     &             'R' /
! ======================================================================
    call jemarq()
    ibid=0
    c16b=(0.d0,0.d0)
    call infmaj()
! ======================================================================
    call getres(result, k8b, nomcmd)
! ======================================================================
! --- DEFINITIONS ------------------------------------------------------
! ======================================================================
    nmgrno = '&&OP0198.NMGRNO'
    tabrev = '        '
    tabmdb = '        '
    tabthr = '        '
    tbinst = '&&OP0198.TBINST'
    tbinth = '&&OP0198.TBINTH'
    tbscrv = '&&OP0198.TBSCRV'
    tbscmb = '&&OP0198.TBSCMB'
    dkma = 0.0d0
    dkmb = 0.0d0
    k1acp = 0.0d0
    k1bcp = 0.0d0
    kal = 0.0d0
    kbl = 0.0d0
! ======================================================================
! --- RECUPERATION DES DONNEES AUTRE QUE K1D ---------------------------
! ======================================================================
    call recupe(noma, ndim, nk1d, lrev, lmdb, &
                matrev, matmdb, deklag, prodef, londef, &
                oridef, profil)
! ======================================================================
! --- VERIFICATION DES DONNEES -----------------------------------------
! ======================================================================
    call veritb(nk1d, ndim, oridef, deklag, profil)
! ======================================================================
! --- RECUPERATION DES TABLES D'INSTANT, D'ABSCISSES CURVILIGNES / -----
! --- COTE REVETEMENT / COTE METAL DE BASE -----------------------------
! ======================================================================
    call recuvl(nbval, tbinst, nbval2, tbinth, norev,&
                tbscrv, nomdb, tbscmb)
! ======================================================================
! --- CREATION DE LA TABLE RESULTAT ------------------------------------
! ======================================================================
    call tbcrsd(result, 'G')
    nval = 8
    if(profil(1:12).eq.'SEMI_ELLIPSE') then
       nval     = 11
    endif
    call tbajpa(result, nval, nomtab, typpar)
! ======================================================================
! --- CREATION DES VECTEURS NECESSAIRES --------------------------------
! ======================================================================
    call wkvect(nmgrno, 'V V K32', nk1d, jnogn)
    call jeveuo(tbinst, 'L', jtbint)
! ======================================================================
! --- ITERATIONS SUR LES DIFFERENTES OCCURENCES DE K1D -----------------
! ======================================================================
    do 10 ik1d = 1, nk1d
! ======================================================================
! --- INITIALISATIONS --------------------------------------------------
! ======================================================================
        dkma  = 0.0d0
        dkmb  = 0.0d0
        dkmc  = 0.0d0
        k1acp = 0.0d0
        k1bcp = 0.0d0
        k1ccp = 0.0d0
        kal   = 0.0d0
        kbl   = 0.0d0
        kcl   = 0.0d0
! ======================================================================
! --- RECUPERATION DES DONNEES ASSOCIEES A LA IK1D OCCURENCE DE K1D ----
! ======================================================================
        call recutb(ik1d, zk32(jnogn-1+ik1d), tabrev, tabmdb, tabthr)
! ======================================================================
! --- ITERATIONS SUR LES INSTANTS MECANIQUES ---------------------------
! ======================================================================
        do 20 itime = 1, nbval
            temps = zr(jtbint-1+itime)
! ======================================================================
! --- RECUPERATION DES CHAMPS MECANIQUES -------------------------------
! ======================================================================
            sigmrv = '&&OP0198.SIGMRV'
            sigmdb = '&&OP0198.SIGMDB'
            call rechmc(ndim, temps, oridef, tabrev, tabmdb,&
                        norev, sigmrv, nomdb, sigmdb)
! ======================================================================
! --- RECUPERATION DES TEMPERATURES AUX POINTES DE LA FISSURE ----------
! ======================================================================
            call rechth(temps, nbval2, tbinth, tabthr, tempa,&
                        tempb)
! ======================================================================
! --- CALCUL DES FACTEURS D'INTENSITE DE CONTRAINTES ELASTIQUES --------
! ======================================================================
            if(profil(1:7).eq.'ELLIPSE') then
               call calck1(norev, nomdb, sigmrv, sigmdb, tbscrv,&
                           tbscmb, prodef, londef, deklag, lrev,&
                           k1a, k1b)
! ======================================================================
! --- CALCUL DES FACTEURS D'INTENSITE DE CONTRAINTES ELASTIQUES --------
! --- METHODE DES COEFFICIENTS D'INFLUENCE -----------------------------
! ======================================================================
            else if(profil(1:12).eq.'SEMI_ELLIPSE') then
               call calc_infl_k1(nomdb, sigmdb, tbscmb, prodef, londef, &
                                 lrev, lmdb, matrev, matmdb, tempa,&
                                 tempb, k1a, k1b, k1c)
            endif
! ======================================================================
! --- AJOUT DE CORRECTION PLASTIQUE AU CALCUL DES FACTEURS -------------
! --- D'INTENSITE DE CONTRAINTES ---------------------------------------
! ======================================================================
            decal=deklag
            if(deklag.ge.0.d0) decal=0.d0
            if(profil(1:12).eq.'SEMI_ELLIPSE') decal=0.d0
            call coplas(tempa, k1a, k1b, k1c, matrev, &
                        lrev, decal, prodef, oridef, profil, &
                        kal, kbl, kcl, dkma, dkmb, &
                        dkmc, k1acp, k1bcp, k1ccp)
! ======================================================================
! --- RECUPERATION DES TEMPERATURES AUX POINTES DE LA FISSURE ----------
! ======================================================================
            rnom(1) = temps
            rnom(2) = k1a
            rnom(3) = k1acp
            rnom(4) = tempa
            rnom(5) = k1b
            rnom(6) = k1bcp
            rnom(7) = tempb
            knom    = zk32(jnogn+ik1d-1)
            nval    = 8
!
            if(profil(1:12).eq.'SEMI_ELLIPSE') then
               nval     = 11
               rnom(8)  = k1c
               rnom(9)  = k1ccp
               rnom(10) = tempa
            endif
!    
            call tbajli(result, nval, nomtab, [ibid], rnom,&
                        [c16b], knom, 0)

! ======================================================================
! --- DESTRUCTION DES CHAMPS DE CONTRAINTES ----------------------------
! ======================================================================
            call jedetr(sigmrv)
            call jedetr(sigmdb)
20      continue
10  continue
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
