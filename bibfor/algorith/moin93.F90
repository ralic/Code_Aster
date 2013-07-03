subroutine moin93(masse, raide, raidfa, nbmoin, matmod,&
                  vefreq)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!     ------------------------------------------------------------------
!           M. CORUS     DATE 4/02/10
!     ------------------------------------------------------------------
!
!     BUT : CALCUL DE MODES DE COUPLAGES,
!                => APPROXIMATION DES MODES D'INTERFACE
!           ON CONSTRUIT UN MODELE SIMPLIFIE DE L'INTERFACE, SUR LA BASE
!           D'UN TREILLIS DE POUTRES. ON CALCUL LES PREMIERS MODES DE CE
!           TREILLIS, QUI SERT A CONSTRUIRE UN SOUS ESPACE PERTINENT
!           POUR LE CALCUL DES MODES D'INTERFACE.
!
!     ------------------------------------------------------------------
!
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
!
#include "asterfort/conint.h"
#include "asterfort/ddllag.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/isdeco.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/modint.h"
#include "asterfort/mstget.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: ibid, ierd, neq, ieq, i1, j1, k1, l1, m1, n1, nbmoin, lindno
    integer :: linddl, lddld, nbnoeu, lprno, nnoint, ipos1, ipos2, numno, nbcmpm
    integer :: nbec, nddlin, lnoint, connec, lconnc, sizeco, lintrf, linlag
    integer :: lipos, lfreq
!
    parameter    (nbcmpm=10)
    integer :: deco(nbcmpm)
    real(kind=8) :: rbid, shift
!
    character(len=8) :: k8b, k8bid, nomma
    character(len=14) :: nume, nume91
    character(len=16) :: nomcmd
    character(len=19) :: raide, masse, solveu, prno, ssami, raiint, raidfa
    character(len=24) :: coint, noddli, matmod, vefreq
!
!     ------------------------------------------------------------------
    call jemarq()
!
    call dismoi('F', 'NB_EC', 'DEPL_R', 'GRANDEUR', nbec,&
                k8bid, ibid)
    call dismoi('F', 'NOM_MAILLA', raide, 'MATR_ASSE', ibid,&
                nomma, ierd)
    call dismoi('F', 'NOM_NUME_DDL', raide, 'MATR_ASSE', ibid,&
                nume, ierd)
    call dismoi('F', 'NB_EQUA', raide, 'MATR_ASSE', neq,&
                k8b, ierd)
    call dismoi('F', 'SOLVEUR', raide, 'MATR_ASSE', ibid,&
                solveu, ierd)
    call dismoi('F', 'NB_NO_MAILLA', nomma, 'MAILLAGE', nbnoeu,&
                k8bid, ierd)
!
!-----------------------------------------------------C
!--                                                 --C
!-- CONSTRUCTION DES MATRICES DU MODELE D'INTERFACE --C
!--                                                 --C
!-----------------------------------------------------C
!
!-- RECUPERATION DE LA DEFINITION DES EQUATIONS
    call dismoi('F', 'PROF_CHNO', nume, 'NUME_DDL', ibid,&
                prno, ierd)
    call jeveuo(jexnum(prno//'.PRNO', 1), 'L', lprno)
!
!-- ALLOCATION ET REMPLISSAGE DU VECTEUR DES INDICES DES DDL D'INTERFACE
    call wkvect('&&MOIN93.IS_DDL_INTERF', 'V V I', neq, lddld)
    call mstget(nomcmd, raide, 'MODE_INTERF', 1, zi(lddld))
    nddlin=0
    nnoint=0
    numno=0
!
    do 10 i1 = 1, nbnoeu
        ipos1=zi(lprno+(i1-1)*(2+nbec))
        ipos2=zi(lprno+(i1-1)*(2+nbec)+1)
        if (ipos1 .gt. 0) then
            do 15 j1 = 1, ipos2
                if (zi(lddld+ipos1-1+j1-1) .gt. 0) then
                    nddlin=nddlin+1
                    if (numno .eq. 0) then
                        numno=1
                        nnoint=nnoint+1
                    endif
                endif
15          continue
            numno=0
        endif
10  end do
!
!-- RECUPERATION DES NOEUDS D'INTERFACE
    noddli='&&MOIN93.NOEUDS_DDL_INT'
    call wkvect(noddli, 'V V I', 9*nnoint, lnoint)
    call wkvect('&&MOIN93.V_IND_DDL_INT', 'V V I', nddlin, linddl)
    call wkvect('&&MOIN93.V_IND_LAG', 'V V I', 2*nddlin, linlag)
    call wkvect('&&MOIN93.IPOS_DDL_INTERF', 'V V I', nnoint, lipos)
    call wkvect('&&MOIN93.DDL_ACTIF_INT', 'V V I', nddlin, lintrf)
!
    k1=0
    numno=0
    m1=0
    do 20 i1 = 1, nbnoeu
        ipos1=zi(lprno+(i1-1)*(2+nbec))
        ipos2=zi(lprno+(i1-1)*(2+nbec)+1)
        if (ipos1 .gt. 0) then
!
            do 25 j1 = 1, ipos2
!
                if (zi(lddld+ipos1-1+j1-1) .gt. 0) then
!-- RECHERCHE DES DDL D'INTERFACE
                    zi(linddl+m1)=ipos1+j1-1
                    m1=m1+1
!-- RECHERCHE DES LAGRANGES ASSOCIES AUX DDL D'INTERFACE
                    call ddllag(nume, ipos1+j1-1, neq, zi(linlag+(m1-1)* 2),&
                                zi(linlag+(m1-1)*2+1))
                    rbid=(zi(linlag+(m1-1)*2)*zi(linlag+(m1-1)*2+1))
                    if (rbid .eq. 0) then
                        call u2mess('F', 'ALGELINE2_4')
                    endif
!
!-- RECHERCHE DES TYPES DES DDLS ACTIFS
                    if (numno .eq. 0) then
                        numno=1
                        zi(lnoint+k1)=i1
                        zi(lnoint+k1+nnoint)=ipos1
                        zi(lnoint+k1+2*nnoint)=6
                        call isdeco(zi(lprno+(i1-1)*(2+nbec)+2), deco, nbcmpm)
                        l1=1
                        do 30,n1=1,6
                        if (deco(n1)*zi(lddld+ipos1-1+n1-1) .gt. 0) then
                            zi(lintrf+m1-1+l1-1)=k1*6+n1
                            l1=l1+1
                        endif
                        zi(lnoint+k1+(2+n1)*nnoint)=n1
!
30                      continue
                        k1=k1+1
!
                    endif
                endif
!
25          continue
            numno=0
        endif
20  end do
!
    call wkvect('&&MOIN93.IND_NOEUD', 'V V I', zi(lnoint+nnoint-1), lindno)
!
!--
!-- CONSTRUCTION DE LA CONNECTIVITE, DU MODELE ET DES MATRICES --C
!--
!
!  LA TAILLE EST LARGEMENT SURESTIMEE : DANS LE CAS QUAD DE DEGRES 2,
!  ON PEUT AVOIR JUSQU'A 24 VOISINS, ON ALLOUE UN NOMBRE MAX DE 35.
!  LA PREMIERE COLONNE DONNE LE NOMBRE DE VOISINS
    sizeco=36*nnoint
    coint='&&MOIN93.CONNEC_INTERF'
    nume91='&&NUME91'
    raiint='&&RAID91'
    ssami='&&MASS91'
!
    call wkvect(coint, 'V V I', sizeco, lconnc)
    call conint(nume, raide, coint, sizeco, connec,&
                noddli, nnoint, nume91, raiint, ssami)
!
!-------------------------------------------------C
!--                                             --C
!-- CALCUL DES MODES DES OPERATEURS D'INTERFACE --C
!--                                             --C
!-------------------------------------------------C
!
    call modint(ssami, raiint, nddlin, nbmoin, shift,&
                matmod, masse, raidfa, neq, coint,&
                noddli, nnoint, vefreq, 1)
    call jeveuo(vefreq, 'L', lfreq)
!
!
    do 60 ieq = 1, nbmoin
        write(6,'(I10,4X,F12.2)')ieq,zr(lfreq+ieq-1)
60  end do
!
!----------------------------------------C
!--                                    --C
!-- DESTRUCTION DES OBJETS TEMPORAIRES --C
!--                                    --C
!----------------------------------------C
!
    call detrsd('SOLVEUR', '&&NUME91')
    call jedetr(nume91(1:14)//'.NUME.REFN')
    call detrsd('MATR_ASSE', ssami)
    call detrsd('MATR_ASSE', raiint)
    call detrsd('NUME_DDL', nume91)
!
    call jedetr('&&MODL91      .MODG.SSNO')
    call jedetr('&&MODL91      .MODG.SSME')
!
    call jedetr('&&MOIN93.IND_NOEUD')
    call jedetr('&&MOIN93.IPOS_DDL_INTERF')
    call jedetr('&&MOIN93.IS_DDL_INTERF')
    call jedetr('&&MOIN93.V_IND_DDL_INT')
    call jedetr('&&MOIN93.V_IND_LAG')
    call jedetr('&&MOIN93.DDL_ACTIF_INT')
!
    call jedetr(noddli)
    call jedetr(coint)
!
    call jedetr('&&MOIN93.NOEU')
    call jedetr('&&MOIN93.ECRITURE.RES')
    call jedetr('&&MOIN93.NOM_PARA')
!
!---------C
!--     --C
!-- FIN --C
!--     --C
!---------C
!
    call jedema()
!
end subroutine
