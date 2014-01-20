subroutine vpnorm(norm, para, lmatr, neq, nbmode,&
                  ddlexc, vecpro, resufr, lmasin, xmastr,&
                  isign, numddl, coef)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/mrmult.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=*) :: norm, para
    integer :: nbmode, neq, lmatr, ddlexc(*)
    real(kind=8) :: vecpro(neq, *), resufr(nbmode, *), xmastr, coef(*)
    logical :: lmasin
!     ------------------------------------------------------------------
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
!     NORMALISATION DE VECTEURS ET DE GRANDEURS MODALES
!     ------------------------------------------------------------------
! IN  NORM   : TYPE DE NORMALISATION
!          = 'AVEC_CMP'
!          = 'MASS_GENE'
!          = 'RIGI_GENE'
!          = 'EUCL', 'EUCL_TRAN', ...
! IN  PARA   : ON REPERCUTE LA NORMALISATION SUR LES PARAMETRES MODAUX
!          = 'OUI' DANS CE CAS ILS DOIVENT DEJA AVOIR ETE CALCULES
!          = 'NON' ON NE NORMALISE QUE LES VECTEURS PROPRES
! IN  LMATR   : DESCRIPTEUR D'UNE MATRICE
! IN  NEQ    : NOMBRE D'EQUATIONS
! IN  NBMODE : NOMBRE DE MODES
! IN  DDLEXC : TABLEAU DES DDL EXCLUS
!          = 0 SI EXCLUS
!          = 1 SI NON EXCLUS
! VAR VECPRO : TABLEAU DES VECTEURS PROPRES
! VAR RESUFR : TABLEAU DES GRANDEURS MODALES RANGEES SELON
!        'FREQ'            , 'OMEGA2'          , 'AMOR_REDUIT'     ,
!        'MASS_GENE'       , 'RIGI_GENE'       , 'AMOR_GENE'       ,
!        'MASS_EFFE_DX'    , 'MASS_EFFE_DY'    , 'MASS_EFFE_DZ'    ,
!        'FACT_PARTICI_DX' , 'FACT_PARTICI_DY' , 'FACT_PARTICI_DZ' ,
!        'MASS_EFFE_UN_DX' , 'MASS_EFFE_UN_DY' , 'MASS_EFFE_UN_DZ'
! IN  LMASIN : CALCUL DES MASSES MODALES UNITAIRES
! IN  XMASTR : MASSE DE LA STRUCTURE
! OUT COEF   : COEFFICIENTS
!     ------------------------------------------------------------------
!
    character(len=24) :: valk
!
    real(kind=8) :: xmn, xx1, xx2, xx3, xnorm
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: ie, im, indg, isign,   numddl
    real(kind=8), pointer :: poi1(:) => null()
    real(kind=8), pointer :: poi2(:) => null()
!
!-----------------------------------------------------------------------
    call jemarq()
    if (norm .eq. 'AVEC_CMP' .or. norm(1:4) .eq. 'EUCL') then
!
!     --- NORMALISATION SUR LES DDL NON EXCLUS
!
        do 2 im = 1, nbmode
            xnorm = 0.0d0
            if (norm(1:4) .eq. 'EUCL') then
                do 4 ie = 1, neq
                    xx1 = vecpro(ie,im) * ddlexc(ie)
                    xnorm = xnorm + xx1*xx1
 4              continue
                xnorm = sqrt(xnorm)
            else
                do 6 ie = 1, neq
                    xx1 = vecpro(ie,im)*ddlexc(ie)
                    if (abs(xnorm) .lt. abs(xx1)) then
                        xnorm = xx1
                    endif
 6              continue
            endif
            xx1 = 1.0d0 / xnorm
            coef(im) = xx1
            do 8 ie = 1, neq
                vecpro(ie,im) = vecpro(ie,im) * xx1
 8          continue
            if (para .eq. 'OUI') then
                xx2 = xx1 * xx1
                resufr(im,4) = resufr(im,4) * xx2
                resufr(im,5) = resufr(im,5) * xx2
!-PROV        RESUFR(IM,6)  = RESUFR(IM,6)  * XX2
                resufr(im,10) = resufr(im,10) * xnorm
                resufr(im,11) = resufr(im,11) * xnorm
                resufr(im,12) = resufr(im,12) * xnorm
            else
                xx2 = xx1 * xx1
                resufr(im,4) = resufr(im,4) * xx2
                resufr(im,5) = resufr(im,5) * xx2
!-PROV        RESUFR(IM,6)  = RESUFR(IM,6)  * XX2
            endif
 2      continue
!
    else if (norm.eq.'MASS_GENE' .or. norm.eq.'RIGI_GENE') then
!
!     --- ON NORMALISE LA MASSE OU LA RAIDEUR GENERALISEE A 1 ---
!
        indg = 4
        if (norm .eq. 'RIGI_GENE') indg=5
        if (para .eq. 'OUI') then
            do 10 im = 1, nbmode
                xmn = resufr(im,indg)
                xx1 = 1.0d0 / xmn
                xx2 = sqrt(xmn)
                xx3 = 1.0d0 / xx2
                resufr(im,4) = resufr(im,4) * xx1
                resufr(im,5) = resufr(im,5) * xx1
                resufr(im,10) = resufr(im,10) * xx2
                resufr(im,11) = resufr(im,11) * xx2
                resufr(im,12) = resufr(im,12) * xx2
                coef(im) = xx3
                do 12 ie = 1, neq
                    vecpro(ie,im) = vecpro(ie,im) * xx3
12              continue
10          continue
        else
            AS_ALLOCATE(vr=poi1, size=neq)
            AS_ALLOCATE(vr=poi2, size=neq)
            do 20 im = 1, nbmode
                do 22 ie = 1, neq
                    poi1(ie) = vecpro(ie,im)
22              continue
                call mrmult('ZERO', lmatr, poi1, poi2, 1,&
                            .true.)
                xmn = 0.0d0
                do 24 ie = 1, neq
                    xmn = xmn + ( poi1(ie) * poi2(ie) )
24              continue
                xx1 = 1.0d0 / sqrt(xmn)
                coef(im) = xx1
                do 26 ie = 1, neq
                    vecpro(ie,im) = vecpro(ie,im) * xx1
26              continue
20          continue
            AS_DEALLOCATE(vr=poi2)
            AS_DEALLOCATE(vr=poi1)
        endif
!
    else
!
        valk = norm
        call utmess('F', 'ALGELINE4_77', sk=valk)
!
    endif
    if (lmasin) then
        do 30 im = 1, nbmode
            resufr(im,13) = resufr(im,7) / xmastr
            resufr(im,14) = resufr(im,8) / xmastr
            resufr(im,15) = resufr(im,9) / xmastr
30      continue
    endif
!
    if (isign .eq. 0) then
    else if (isign .eq. 1) then
        do 100 im = 1, nbmode
            xx1 = vecpro(numddl,im)
            if (xx1 .lt. 0.0d0) then
                coef(im) = -coef(im)
                do 102 ie = 1, neq
                    vecpro(ie,im) = -vecpro(ie,im)
102              continue
                if (para .eq. 'OUI') then
                    resufr(im,10) = -resufr(im,10)
                    resufr(im,11) = -resufr(im,11)
                    resufr(im,12) = -resufr(im,12)
                endif
            endif
100      continue
    else if (isign .eq. -1) then
        do 110 im = 1, nbmode
            xx1 = vecpro(numddl,im)
            if (xx1 .gt. 0.0d0) then
                coef(im) = -coef(im)
                do 112 ie = 1, neq
                    vecpro(ie,im) = -vecpro(ie,im)
112              continue
                if (para .eq. 'OUI') then
                    resufr(im,10) = -resufr(im,10)
                    resufr(im,11) = -resufr(im,11)
                    resufr(im,12) = -resufr(im,12)
                endif
            endif
110      continue
    endif
!
    call jedema()
end subroutine
