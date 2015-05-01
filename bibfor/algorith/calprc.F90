subroutine calprc(nomres, classe, basmod, nommat)
    implicit none
#include "jeveux.h"
#include "asterfort/copmod.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mcmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/mtexis.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/zeclag.h"
    character(len=24) :: nomres
    character(len=1) :: classe
    character(len=8) :: basmod
    character(len=19) :: nommat
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!  BUT : < PROJECTION MATRICE SUR BASE QUELCONQUE >
!
!        CONSISTE A PROJETER UNE MATRICE ASSSEMBLEE COMPLEXE
!        SUR UNE BASE QUELCONQUE (PAS DE PROPRIETE D'ORTHOGONALITE)
!
!        LA MATRICE RESULTAT EST SYMETRIQUE ET STOCKEE TRIANGLE SUP
!
!-----------------------------------------------------------------------
!
! NOMRES /O/ : NOM K19 DE LA MATRICE CARREE RESULTAT
! CLASSE /I/ : CLASSE DE LA BASE JEVEUX DE L'OBJET RESULTAT
! BASMOD /I/ : NOM UTILISATEUR DE LA BASE MODALE DE PROJECTION
! NOMMAT /I/ : NOM UTITISATEUR DE LA MATRICE A PROJETER (RAIDEUR,MASSE)
!
!
!
!
    character(len=6) :: pgc
    character(len=14) :: num
    character(len=24) :: valk
    complex(kind=8) :: xprod, dcmplx, cbid
    integer :: ldref, nbdef, ntail, ldres, ier, lmat, neq
    integer :: iddeeq, idbase, ltvec1, ltvec2, i, j, k, iad, lddes
    cbid = dcmplx(0.d0, 0.d0)
!
!
!-----------------------------------------------------------------------
    data pgc/'CALPRC'/
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
    call dismoi('NB_MODES_TOT', basmod, 'RESULTAT', repi=nbdef)
!
! --- ALLOCATION DE LA MATRICE RESULTAT
!
    ntail = nbdef* (nbdef+1)/2
    call wkvect(nomres(1:18)//'_VALE', classe//' V C', ntail, ldres)
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
    call dismoi('NB_EQUA', nommat(1:8), 'MATR_ASSE', repi=neq)
    call dismoi('NOM_NUME_DDL', nommat(1:8), 'MATR_ASSE', repk=num)
    call jeveuo(num//'.NUME.DEEQ', 'L', iddeeq)
!
    call wkvect('&&'//pgc//'.BASEMO', 'V V R', nbdef*neq, idbase)
    call copmod(basmod, bmodr=zr(idbase), numer=num)
!
!
! --- ALLOCATION VECTEUR DE TRAVAIL
!
    call wkvect('&&'//pgc//'.VECT1', 'V V C', neq, ltvec1)
    call wkvect('&&'//pgc//'.VECT2', 'V V C', neq, ltvec2)
!
! --- PROJECTION SUR DEFORMEES
!
    do i = 1, nbdef
!
! ----- CALCUL PRODUIT MATRICE DEFORMEE
!
        do j = 1, neq
            zc(ltvec1+j-1)=dcmplx(zr(idbase+(i-1)*neq+j-1),0.d0)
        end do
        call mcmult('ZERO', lmat, zc(ltvec1), zc(ltvec2), 1,&
                    .true._1)
        call zeclag(zc(ltvec2), neq, zi(iddeeq))
        do j = 1, neq
        end do
!
! ----- PRODUIT AVEC LA DEFORMEE COURANTE
!
        xprod=dcmplx(0.d0,0.d0)
        do j = 1, neq
            xprod=xprod+ zc(ltvec2-1+j)*dcmplx(zr(idbase+(i-1)*neq-1+&
            j),0.d0)
        end do
!
        iad = i*(i+1)/2
        zc(ldres+iad-1) = xprod
!
! ----- PRODUIT AVEC DEFORMEES D'ORDRE SUPERIEURE
!
        if (i .lt. nbdef) then
            do j = i+1, nbdef
                xprod=dcmplx(0.d0,0.d0)
                do k = 1, neq
                    xprod=xprod+ zc(ltvec2-1+k)*dcmplx(zr(idbase+(j-1)&
                    *neq-1+k),0.d0)
                end do
                iad = i+(j-1)*j/2
                zc(ldres+iad-1) = xprod
            end do
        endif
!
    end do
!
    call jedetr('&&'//pgc//'.VECT1')
    call jedetr('&&'//pgc//'.VECT2')
    call jedetr('&&'//pgc//'.BASEMO')
!
! --- CREATION DU .DESC
!
    call wkvect(nomres(1:18)//'_DESC', 'G V I', 3, lddes)
    zi(lddes) = 2
    zi(lddes+1) = nbdef
    zi(lddes+2) = 2
!
    call jedema()
end subroutine
