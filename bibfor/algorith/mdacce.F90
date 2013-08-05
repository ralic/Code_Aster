subroutine mdacce(typbas, neqgen, pulsa2, masgen, descm,&
                  riggen, descr, fexgen, lamor, amogen,&
                  desca, work1, depgen, vitgen, accgen)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/pmavec.h"
#include "asterfort/resoud.h"
#include "asterfort/rrlds.h"
#include "asterfort/trlds.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
    integer :: neqgen, descm, descr, desca
    real(kind=8) :: masgen(*), riggen(*), fexgen(*), amogen(*)
    real(kind=8) :: depgen(*), vitgen(*), accgen(*)
    real(kind=8) :: work1(*), pulsa2(*)
    character(len=16) :: typbas
    logical :: lamor
    complex(kind=8) :: cbid
!-----------------------------------------------------------------------
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
!     CALCUL DE L'ACCELERATION (EQUATION D'EQUILIBRE)
!     ------------------------------------------------------------------
! IN  : TYPBAS : TYPE DE LA BASE 'MODE_MECA' 'BASE_MODA' 'MODELE_GENE'
! IN  : NEQGEN : NOMBRE DE MODES
! IN  : PULSA2 : PULSATIONS MODALES AU CARREES
! IN  : MASGEN : MASSES GENERALISEES ( TYPBAS = 'MODE_MECA' )
!                MATRICE DE MASSE GENERALISEE ( TYPBAS = 'BASE_MODA' )
! IN  : DESCM : DESCRIPTEUR DE LA MATRICE DE MASSE
! IN  : RIGGEN : RAIDEURS GENERALISES ( TYPBAS = 'MODE_MECA' )
!                MATRICE DE RAIDEUR GENERALISE ( TYPBAS = 'BASE_MODA' )
! IN  : DESCR : DESCRIPTEUR DE LA MATRICE DE RIGIDITE
! IN  : FEXGEN : FORCES EXTERIEURES GENERALISEES
! IN  : LAMOR  : AMORTISSEMENT SOUS FORME D'UNE LISTE DE REELS
! IN  : AMOGEN : AMORTISSEMENTS REDUITS ( LAMOR = .TRUE. )
!                MATRICE D'AMORTISSEMENT ( LAMOR = .FALSE. )
! IN  : DESCA : DESCRIPTEUR DE LA MATRICE D'AMORTISSEMENT
! IN  : WORK1  : VECTEUR DE TRAVAIL
! IN  : DEPGEN : DEPLACEMENTS GENERALISES
! IN  : VITGEN : VITESSES GENERALISEES
! OUT : ACCGEN : ACCELERATIONS GENERALISEES
! ----------------------------------------------------------------------
!
!
!
!
    real(kind=8) :: x1, x2
    character(len=19) :: matr
!
!-----------------------------------------------------------------------
    integer :: im, iret, jmass
!-----------------------------------------------------------------------
    if (typbas(1:9) .eq. 'MODE_MECA' .or. typbas(1:9) .eq. 'MODE_GENE') then
        if (lamor) then
            do 100 im = 1, neqgen
                x1 = fexgen(im) / masgen(im)
                x2 = pulsa2(im)*depgen(im) + amogen(im)*vitgen(im)
                accgen(im) = x1 - x2
100          continue
        else
            call pmavec('ZERO', neqgen, amogen, vitgen, work1)
            do 110 im = 1, neqgen
                x1 = fexgen(im) / masgen(im)
                x2 = pulsa2(im)*depgen(im) + work1(im)/masgen(im)
                accgen(im) = x1 - x2
110          continue
        endif
!
    else if (typbas(1:9).eq.'BASE_MODA') then
        call jeexin('&&MDACCE.MASS', iret)
        if (iret .eq. 0) then
            call wkvect('&&MDACCE.MASS', 'V V R8', neqgen*neqgen, jmass)
            call dcopy(neqgen*neqgen, masgen, 1, zr(jmass), 1)
            call trlds(zr(jmass), neqgen, neqgen, iret)
        else
            call jeveuo('&&MDACCE.MASS', 'E', jmass)
        endif
        if (lamor) then
            call pmavec('ZERO', neqgen, masgen, vitgen, work1)
            do 120 im = 1, neqgen
                work1(im) = amogen(im)*work1(im)
120          continue
        else
            call pmavec('ZERO', neqgen, amogen, vitgen, work1)
        endif
        call pmavec('CUMU', neqgen, riggen, depgen, work1)
        do 130 im = 1, neqgen
            accgen(im) = fexgen(im) - work1(im)
130      continue
!        CALL DCOPY(NEQGEN*NEQGEN,MASGEN,1,ZR(JMASS),1)
!        CALL TRLDS(ZR(JMASS),NEQGEN,NEQGEN,IRET)
        call rrlds(zr(jmass), neqgen, neqgen, accgen, 1)
!
    else if (typbas(1:11).eq.'MODELE_GENE') then
        if (desca .ne. 0) then
            call mrmult('ZERO', desca, vitgen, work1, 1,&
                        .false.)
            call mrmult('CUMU', descr, depgen, work1, 1,&
                        .false.)
        else
            call mrmult('ZERO', descr, depgen, work1, 1,&
                        .false.)
        endif
        do 140 im = 1, neqgen
            accgen(im) = fexgen(im) - work1(im)
140      continue
!
        ASSERT(descm.ne.0)
        matr = zk24(zi(descm+1))(1:19)
        call resoud(matr, ' ', ' ', ' ', 1,&
                    ' ', ' ', ' ', accgen, cbid,&
                    ' ', .true., 0, iret)
    endif
!
end subroutine
