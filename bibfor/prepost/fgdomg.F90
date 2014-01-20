subroutine fgdomg(method, nommat, nomnap, nomfon, valmin,&
                  valmax, ncyc, dommag)
!       ================================================================
    implicit none
!
#include "jeveux.h"
#include "asterfort/fgdoba.h"
#include "asterfort/fgdohs.h"
#include "asterfort/fgdoma.h"
#include "asterfort/fgdowh.h"
#include "asterfort/fgtaep.h"
#include "asterfort/fgtaes.h"
#include "asterfort/jedetr.h"
#include "asterfort/rcpare.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=*) :: method
    character(len=8) :: nommat, nomnap, nomfon
    real(kind=8) :: valmin(*), valmax(*), dommag
    integer :: ncyc
!
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
!       ----------------------------------------------------------------
!       CALCUL DU DOMMAGE PAR DIFFERENTES METHODES
!       ----------------------------------------------------------------
!       IN  METHOD  METHODE DE CALCUL  DE DOMMAGE EMPLOYEE
!                       /WOHLER
!                       /MANSON_COFFIN
!                       /TAHERI_MANSON
!                       /TAHERI_MIXTE
!           NOMMAT  NOM DU CHAM_MATER
!           NOMNAP  NOM DE LA NAPPE POUR LOI DE TAHERI
!           NOMFON  NOM DE LA FONCTION POUR LOI DE TAHERI
!           CYCLE   TABLE DES CYCLES DETECTES (INDICES DEB ET FIN)
!           NCYC    NOMBRE  DE  CYCLE
!       OUT DOMMAG  VALEUR DU DOMMAGE
!       ----------------------------------------------------------------
    integer :: icodwo, icodba, icodhs
    character(len=8) :: cara
    character(len=10) :: pheno
    character(len=16) :: k16b
    logical :: lke, lhaigh
!
!-----------------------------------------------------------------------
    integer :: i, ivcorr,  ivke
    real(kind=8), pointer :: vdommag(:) => null()
!-----------------------------------------------------------------------
    dommag = 0.d0
    pheno = 'FATIGUE'
    AS_ALLOCATE(vr=vdommag, size=ncyc)
    lke = .false.
    lhaigh = .false.
    ivke = 0
    ivcorr = 0
!
    if (method .eq. 'WOHLER') then
        cara = 'WOHLER'
        call rcpare(nommat, pheno, cara, icodwo)
        cara = 'A_BASQUI'
        call rcpare(nommat, pheno, cara, icodba)
        cara = 'A0'
        call rcpare(nommat, pheno, cara, icodhs)
        if (icodwo .eq. 0) then
            call fgdowh(nommat, ncyc, valmin, valmax, lke,&
                        zr(ivke), lhaigh, zr(ivcorr),vdommag)
        else if (icodba.eq.0) then
            call fgdoba(nommat, ncyc, valmin, valmax, lke,&
                        zr(ivke), lhaigh, zr(ivcorr),vdommag)
        else if (icodhs.eq.0) then
            call fgdohs(nommat, ncyc, valmin, valmax, lke,&
                        zr(ivke), lhaigh, zr(ivcorr),vdommag)
        endif
    else if (method.eq.'MANSON_COFFIN') then
        call fgdoma(nommat, ncyc, valmin, valmax,vdommag)
    else if (method.eq.'TAHERI_MANSON') then
        call fgtaep(nommat, nomfon, nomnap, ncyc, valmin,&
                    valmax,vdommag)
    else if (method.eq.'TAHERI_MIXTE') then
        call fgtaes(nommat, nomnap, ncyc, valmin, valmax,&
vdommag)
    else
        k16b = method(1:16)
        call utmess('F', 'PREPOST_4', sk=k16b)
    endif
!
    do 100 i = 1, ncyc
        dommag = dommag + vdommag(i)
100  end do
!
    AS_DEALLOCATE(vr=vdommag)
end subroutine
