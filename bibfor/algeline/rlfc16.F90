subroutine rlfc16(nommat, neq, cxsol, nbsol, typsym)
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mlnmin.h"
#include "asterfort/mltdca.h"
#include "asterfort/wkvect.h"
    integer :: neq, nbsol, typsym
    character(len=*) :: nommat
    complex(kind=8) :: cxsol(neq, *)
!     ------------------------------------------------------------------
! person_in_charge: olivier.boiteau at edf.fr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     RESOLUTION DU SYSTEME A COEFFICIENTS REELS:  A * X = B
!     LA MATRICE EST SYMETRIQUE ET A ETE FACTORISEE SOUS FORME L*D*LT
!     LA RESOLUTION EST EN PLACE
!
!     ON PEUT RESOUDRE SUR UNE SOUS-MATRICE DE A :
!     ON PREND LES NEQ PREMIERES LIGNES ET COLONNES (NEQ PEUT ETRE
!     INFERIEUR A LA DIMENSION DE LA MATRICE).
!
!     ON PEUT RESOUDRE NBSOL SYSTEMES D'UN COUP A CONDITION
!     QUE LES VECTEURS SOIENT CONSECUTIFS EN MEMOIRE
!     ------------------------------------------------------------------
!
! IN  NOMMAT  :    : NOM UTILISATEUR DE LA MATRICE A FACTORISER
! IN  NEQ     : IS : NOMBRE D'EQUATIONS PRISES EN COMPTE
! IN  NBSOL   : IS : NOMBRE DE SOLUTIONS / SECONDS MEMBRES
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    character(len=24) :: factol, factou
    character(len=24) :: nomp01, nomp02, nomp03, nomp04, nomp05, nomp06, nomp07
    character(len=24) :: nomp08, nomp09, nomp10, nomp11, nomp12, nomp13, nomp14
    character(len=24) :: nomp15, nomp16, nomp17, nomp18, nomp19, nomp20
!     -------------------------------------------------- POINTEURS
    integer :: pointr, desc
    integer :: nouv, anc, supnd
    integer :: seq, adress, lgsn
    integer :: decal, global
    integer :: ncbloc, lgbloc, nbloc, nbsn, ad, trav
    integer :: lgblma, points
    integer ::  i
    character(len=14) :: nu
!
!     ------------------------------------------------------------------
    data factol/'                   .VALF'/
    data factou/'                   .WALF'/
!     ------------------------------------------------------------------
    call jemarq()
!
    call dismoi('NOM_NUME_DDL', nommat, 'MATR_ASSE', repk=nu)
    factol(1:19) = nommat
    factou(1:19) = nommat
    call mlnmin(nu, nomp01, nomp02, nomp03, nomp04,&
                nomp05, nomp06, nomp07, nomp08, nomp09,&
                nomp10, nomp11, nomp12, nomp13, nomp14,&
                nomp15, nomp16, nomp17, nomp18, nomp19,&
                nomp20)
!                                ALLOCATION DES POINTEURS ENTIERS
    call jeveuo(nomp01, 'L', desc)
    call jeveuo(nomp03, 'L', adress)
    call jeveuo(nomp04, 'L', supnd)
    call jeveuo(nomp20, 'L', seq)
    call jeveuo(nomp16, 'L', lgbloc)
    call jeveuo(nomp17, 'L', ncbloc)
    call jeveuo(nomp18, 'L', decal)
    call jeveuo(nomp08, 'L', lgsn)
    call jeveuo(nomp14, 'L', anc)
    call jeveuo(nomp19, 'L', nouv)
    nbsn = zi(desc+1)
    nbloc= zi(desc+2)
    lgblma=0
    do 1 i = 0, nbloc-1
        if (zi(lgbloc+i) .gt. lgblma) lgblma = zi(lgbloc+i)
  1 end do
    call wkvect('&&RLFC16.ALLEUR.VALF ', ' V V C ', lgblma, points)
!
!                                ALLOCATION TABLEAU REEL PROVISOIRE
    call wkvect('&&RLFC16.POINTER.REELS ', ' V V C ', neq, pointr)
    call wkvect('&&RLFC16.POINTER.ADRESSE', 'V V I', neq, ad)
    call wkvect('&&RLFC16.POINTER.TRAVAIL', 'V V C', neq, trav)
!
    call jeveuo(nu//'.MLTF.GLOB', 'L', global)
!
    call jedetr('&&RLFC16.ALLEUR.VALF ')
    do 110 i = 1, nbsol
        call mltdca(nbloc, zi(lgbloc), zi(ncbloc), zi(decal), zi(seq),&
                    nbsn, neq, zi(supnd), zi(adress), zi4(global),&
                    zi(lgsn), factol, factou, cxsol(1, i), zc(pointr),&
                    zi(nouv), zi(anc), zi(ad), zc(trav), typsym)
110 end do
!
    call jedetr('&&RLFC16.POINTER.ADRESSE')
    call jedetr('&&RLFC16.POINTER.TRAVAIL')
    call jedetr('&&RLFC16.POINTER.REELS ')
    call jedetr('&&RLFC16.POINTEUR.SUPN')
    call jedetr('&&RLFC16.POINTEUR.ANC ')
    call jedetr('&&RLFC16.POINTEUR.NOUV')
    call jedetr('&&RLFC16.POINTEUR.SEQ ')
    call jedetr('&&RLFC16.POINTEUR.LGSN')
    call jedetr('&&RLFC16.POINTEUR.ADRE')
    call jedetr('&&RLFC16.POINTEUR.LGBL')
    call jedetr('&&RLFC16.POINTEUR.NCBL')
    call jedetr('&&RLFC16.POINTEUR.DECA')
    call jedema()
end subroutine
