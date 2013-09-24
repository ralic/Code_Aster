subroutine calpro(nomres, classe, basmod, nommat)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
!***********************************************************************
! P. RICHARD     DATE 23/05/91
!-----------------------------------------------------------------------
!
!  BUT : < PROJECTION MATRICE SUR BASE QUELCONQUE >
!
!        CONSISTE A PROJETER UNE MATRICE ASSSEMBLEE SUR UNE BASE
!        QUELCONQUE (PAS DE PROPRIETE D'ORTHOGONALITE)
!
!        LA MATRICE RESULTAT EST SYMETRIQUE ET STOCKEE TRIANGLE SUP
!
!-----------------------------------------------------------------------
!
! NOMRES /I/ : NOM K19 DE LA MATRICE CARREE RESULTAT
! CLASSE /I/ : CLASSE DE LA BASE JEVEUX DE L'OBJET RESULTAT
! BASMOD /I/ : NOM UT DE LA BASE MODALE DE PROJECTION
! NOMMAT /I/ : NOM UT DE LA MATRICE A PROJETER (RAIDEUR,MASSE)
!
!
!
!
#include "jeveux.h"
#include "asterfort/copmod.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mtexis.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/zerlag.h"
#include "blas/ddot.h"
    character(len=1) :: classe, typ1
    character(len=6) :: pgc
    character(len=8) :: basmod, k8bid
    character(len=19) :: nommat
    character(len=14) :: num
    character(len=24) :: nomres
    character(len=24) :: valk
    character(len=24) :: nomcha
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iad, ibid, idbase, iddeeq, ier, iret
    integer :: j, lddes, ldref, ldres, lmat, ltvec1, nbdef
    integer :: neq, ntail
    real(kind=8) :: xprod
    complex(kind=8) :: cbid
!-----------------------------------------------------------------------
    data pgc/'CALPRO'/
!-----------------------------------------------------------------------
!
! --- CREATION DU .REFE
!
    call jemarq()
    call wkvect(nomres(1:18)//'_REFE', 'G V K24', 2, ldref)
    zk24(ldref) = basmod
    zk24(ldref+1) = nommat(1:8)
!
! --- RECUPERATION DES DIMENSIONS DE LA BASE MODALE
!
    call dismoi('F', 'NB_MODES_TOT', basmod, 'RESULTAT', nbdef,&
                k8bid, ier)
!
! ----VERIFICATION DU TYPE DES VECTEURS PROPRES DANS LA BASE
!
    call rsexch('F', basmod, 'DEPL', 1, nomcha,&
                iret)
    call jelira(nomcha(1:19)//'.VALE', 'TYPE', cval=typ1)
    if (typ1 .eq. 'C') then
        valk = basmod
        call utmess('F', 'ALGORITH12_16', sk=valk)
    endif
!
! --- ALLOCATION DE LA MATRICE RESULTAT
!
    ntail = nbdef* (nbdef+1)/2
    call wkvect(nomres(1:18)//'_VALE', classe//' V R', ntail, ldres)
!
! --- CONTROLE D'EXISTENCE DE LA MATRICE
!
    call mtexis(nommat(1:8), ier)
    if (ier .eq. 0) then
        valk = nommat(1:8)
        call utmess('E', 'ALGORITH12_39', sk=valk)
    endif
!
! --- ALLOCATION DESCRIPTEUR DE LA MATRICE
!
    call mtdscr(nommat(1:8))
    call jeveuo(nommat(1:19)//'.&INT', 'E', lmat)
!
! --- RECUPERATION NUMEROTATION ET NB EQUATIONS
!
    call dismoi('F', 'NB_EQUA', nommat(1:8), 'MATR_ASSE', neq,&
                k8bid, iret)
    call dismoi('F', 'NOM_NUME_DDL', nommat(1:8), 'MATR_ASSE', ibid,&
                num, iret)
    call jeveuo(num//'.NUME.DEEQ', 'L', iddeeq)
!
    call wkvect('&&CALPRO.BASEMO', 'V V R', nbdef*neq, idbase)
    call copmod(basmod, 'DEPL', neq, num, nbdef,&
                'R', zr(idbase), [cbid])
!
!
! --- ALLOCATION VECTEUR DE TRAVAIL
!
    call wkvect('&&'//pgc//'.VECT1', 'V V R', neq, ltvec1)
!
! --- PROJECTION SUR DEFORMEES
!
    do 10 i = 1, nbdef
!
! ----- CALCUL PRODUIT MATRICE DEFORMEE
!
        call mrmult('ZERO', lmat, zr(idbase+(i-1)*neq), zr(ltvec1), 1,&
                    .true.)
        call zerlag(neq, zi(iddeeq), vectr=zr(ltvec1))
!
! ----- PRODUIT AVEC LA DEFORMEE COURANTE
!
        xprod= ddot(neq,zr(ltvec1),1,zr(idbase+(i-1)*neq),1)
        iad = i*(i+1)/2
        zr(ldres+iad-1) = xprod
!
! ----- PRODUIT AVEC DEFORMEES D'ORDRE SUPERIEURE
!
        if (i .lt. nbdef) then
            do 20 j = i+1, nbdef
                xprod= ddot(neq,zr(ltvec1),1,zr(idbase+(j-1)*neq),1)
                iad = i+(j-1)*j/2
                zr(ldres+iad-1) = xprod
20          continue
        endif
!
10  end do
!
    call jedetr('&&'//pgc//'.VECT1')
!
! --- CREATION DU .DESC
!
    call wkvect(nomres(1:18)//'_DESC', 'G V I', 3, lddes)
    zi(lddes) = 2
    zi(lddes+1) = nbdef
    zi(lddes+2) = 2
!
!
! --- MENAGE
!
    call jedetr('&&CALPRO.BASEMO')
!
    call jedema()
end subroutine
