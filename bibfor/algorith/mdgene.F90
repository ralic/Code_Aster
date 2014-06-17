subroutine mdgene(basemo, nbmode, numgen, masgen, riggen,&
                  amogen, nexcit, jvec, ier)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    integer :: nbmode, nexcit, jvec, ier
    character(len=8) :: basemo, masgen, riggen, amogen
    character(len=14) :: numgen
! ----------------------------------------------------------------------
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
!
!     VERIFICATION DES DONNEES GENERALISEES
!     ------------------------------------------------------------------
! IN  : BASEMO : NOM DU CONCEPT BASE MODALE
! IN  : NBMODE : NOMBRE DE MODES
! IN  : NUMGEN : NOM DU CONCEPT NUMEROTATION GENERALISEE
! IN  : MASGEN : NOM DU CONCEPT MASSE GENERALISEE
! IN  : RIGGEN : NOM DU CONCEPT RAIDEUR GENERALISEE
! IN  : AMOGEN : NOM DU CONCEPT AMORTISSEMENT GENERALISE
! IN  : JVEC   : ADRESSE JEVEUX DES NOMS DES VECTEURS GENERALISES
! OUT : IER    : CODE RETOUR
! ----------------------------------------------------------------------
!
!
!
!
    integer :: nvec1, nvec2
    character(len=8) :: base1, base2, k8b, vecgen
    character(len=14) :: nu1gen, nu2gen, nu3gen, nu4gen, k14b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, jdes1,  jref1, jref2
    integer, pointer :: des2(:) => null()
!-----------------------------------------------------------------------
    data k8b /'        '/
    data k14b /'              '/
!-----------------------------------------------------------------------
!
    call jemarq()
    ier = 0
!
    if (numgen .eq. k14b) then
!
!     --- SI MATR_GENE (STOCKAGE PLEIN) ---
!
        call jeveuo(masgen//'           .REFA', 'L', jref1)
        call jeveuo(riggen//'           .REFA', 'L', jref2)
        base1 = zk24(jref1)(1:8)
        base2 = zk24(jref2)(1:8)
        if (base1 .ne. basemo) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_42')
        endif
        if (base2 .ne. basemo) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_42')
        endif
!
        call jeveuo(masgen//'           .DESC', 'L', jdes1)
        call jeveuo(riggen//'           .DESC', 'L', vi=des2)
        nvec1 = zi(jdes1+1)
        nvec2 = des2(2)
        if (nvec1 .ne. nbmode) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_43')
        endif
        if (nvec2 .ne. nbmode) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_43')
        endif
!
        if (amogen .ne. k8b) then
            call jeveuo(amogen//'           .REFA', 'L', jref1)
            base1 = zk24(jref1)(1:8)
            if (base1 .ne. basemo) then
                ier = ier + 1
                call utmess('E', 'ALGORITH5_42')
            endif
            call jeveuo(amogen//'           .DESC', 'L', jdes1)
            nvec1 = zi(jdes1+1)
            if (nvec1 .ne. nbmode) then
                ier = ier + 1
                call utmess('E', 'ALGORITH5_43')
            endif
        endif
!
    else if (basemo.eq.k8b) then
!
!     --- SI MATR_ASSE_GENE_R (STOCKAGE LIGNE DE CIEL) ---
!
        call jeveuo(masgen//'           .REFA', 'L', jref1)
        call jeveuo(riggen//'           .REFA', 'L', jref2)
        nu1gen = zk24(jref1+1)(1:14)
        nu2gen = zk24(jref2+1)(1:14)
        if (nu1gen .ne. numgen) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_44')
        endif
        if (nu2gen .ne. numgen) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_44')
        endif
!
        if (amogen .ne. k8b) then
            call jeveuo(amogen//'           .REFA', 'L', jref1)
            nu3gen = zk24(jref1+1)(1:14)
            if (nu3gen .ne. numgen) then
                ier = ier + 1
                call utmess('E', 'ALGORITH5_44')
            endif
        endif
!
        if (nexcit .ne. 0) then
            do 10 i = 1, nexcit
                vecgen = zk8(jvec-1+i)
                call jeveuo(vecgen//'           .REFE', 'L', jref1)
                nu4gen = zk24(jref1+1)(1:14)
                if (nu4gen .ne. numgen) then
                    ier = ier + 1
                    call utmess('E', 'ALGORITH5_45')
                endif
10          continue
        endif
!
    endif
!
    call jedema()
end subroutine
