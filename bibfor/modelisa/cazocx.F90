subroutine cazocx(char, nomo, motfac, izone)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/exixfe.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=8) :: char, nomo
    character(len=16) :: motfac
    integer :: izone
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEM - LECTURE DONNEES)
!
! LECTURE DES PRINCIPALES CARACTERISTIQUES DU CONTACT PAR ZONE
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMO   : NOM DU MODELE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  IZONE  : INDICE POUR LIRE LES DONNEES DANS AFFE_CHAR_MECA
!
!
!
!
    character(len=24) :: defico
    character(len=24) :: modcon, caraxf
    integer :: jmoco, jcmxf
    integer :: zcmxf
    character(len=16) :: integ, algola, glis, rela
    character(len=16) :: algoc, algof, staco0
    integer :: noc, paring
    real(kind=8) :: coefcr, coefcp
    real(kind=8) :: coeffr, coeffp
    real(kind=8) :: algocr, algofr
    real(kind=8) :: coefff, reacsi, coef, tolj
    character(len=16) :: valk(2)
    integer :: iret
    aster_logical :: lfrot
    integer, pointer :: xfem_cont(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    tolj = 0.d0
    coefcr = 100.d0
    coefcp = 100.d0
    coeffr = 100.d0
    coeffp = 100.d0
    algocr = 0.d0
    algofr = 0.d0
    coefff = 0.d0
    reacsi = 0.d0
    integ = 'FPG4'
    algola = 'NON'
    algoc = 'STANDARD'
    algof = 'STANDARD'
    lfrot = .false.
    lfrot = cfdisl(defico,'FROTTEMENT')
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    caraxf = defico(1:16)//'.CARAXF'
    modcon = defico(1:16)//'.MODELX'
!
    call jeveuo(caraxf, 'E', jcmxf)
    call jeveuo(modcon, 'E', jmoco)
!
    zcmxf = cfmmvd('ZCMXF')
!
! --- TEST MODELE CORRECT
!
    call exixfe(nomo, iret)
!
    if (iret .eq. 0) then
        valk(1) = nomo
        call utmess('F', 'XFEM2_8', sk=valk(1))
    else
        call jeveuo(nomo(1:8)//'.XFEM_CONT', 'L', vi=xfem_cont)
        if (xfem_cont(1) .eq. 0) then
            valk(1) = nomo
            call utmess('F', 'XFEM2_9', sk=valk(1))
        endif
    endif
!
! --- STOCKAGE DU NOM DU MODELE
!
    zk8(jmoco) = nomo
!
! --- TYPE INTEGRATION
!
    call getvtx(motfac, 'INTEGRATION', iocc=izone, scal=integ, nbret=noc)
    if (integ(1:5) .eq. 'NOEUD') then
        zr(jcmxf+zcmxf*(izone-1)+1-1) = 1.d0
    else if (integ(1:5) .eq. 'GAUSS') then
        call getvis(motfac, 'ORDRE_INT', iocc=izone, scal=paring, nbret=noc)
        zr(jcmxf+zcmxf*(izone-1)+1-1) = 10.d0*paring + 2.d0
    else if (integ(1:7) .eq. 'SIMPSON') then
        call getvis(motfac, 'ORDRE_INT', iocc=izone, scal=paring, nbret=noc)
        zr(jcmxf+zcmxf*(izone-1)+1-1) = 10.d0*paring + 3.d0
    else if (integ(1:6) .eq. 'NCOTES') then
        call getvis(motfac, 'ORDRE_INT', iocc=izone, scal=paring, nbret=noc)
        zr(jcmxf+zcmxf*(izone-1)+1-1) = 10.d0*paring + 4.d0
    else
        ASSERT(.false.)
    endif
!
! --- OPTIONS ALGORITHME CONTACT
!
    call getvtx(motfac, 'ALGO_CONT', iocc=izone, scal=algoc, nbret=noc)
    if (algoc(1:10) .eq. 'STANDARD') then
        call getvr8(motfac, 'COEF_CONT', iocc=izone, scal=coef, nbret=noc)
        coefcr = coef
        coefcp = 0.d0
        algocr = 1.d0
    else if (algoc(1:14) .eq. 'PENALISATION') then
        call getvr8(motfac, 'COEF_PENA_CONT', iocc=izone, scal=coefcp, nbret=noc)
        coefcr = 0.d0
        algocr = 2.d0
    else if (algoc(1:3) .eq. 'CZM') then
        algocr = 3.d0
        lfrot = .false.
    else
        ASSERT(.false.)
    endif
    zr(jcmxf+zcmxf*(izone-1)+2-1) = coefcr
    zr(jcmxf+zcmxf*(izone-1)+12-1) = coefcp
    zr(jcmxf+zcmxf*(izone-1)+11-1) = algocr
!
! --- OPTIONS ALGORITHME FROTTEMENT
!
    if (lfrot) then
        call getvtx(motfac, 'ALGO_FROT', iocc=izone, scal=algof, nbret=noc)
        if (algof(1:10) .eq. 'STANDARD') then
            call getvr8(motfac, 'COEF_FROT', iocc=izone, scal=coef, nbret=noc)
            coeffr = coef
            coeffp = 0.d0
            algofr = 1.d0
        else if (algof(1:14) .eq. 'PENALISATION') then
            call getvr8(motfac, 'COEF_PENA_FROT', iocc=izone, scal=coeffp, nbret=noc)
            coeffr = 0.d0
            algofr = 2.d0
        else
            ASSERT(.false.)
        endif
    else
        coeffr = 0.d0
        coeffp = 0.d0
        algofr = 0.d0
    endif
!
    zr(jcmxf+zcmxf*(izone-1)+3-1) = coeffr
    zr(jcmxf+zcmxf*(izone-1)+14-1) = coeffp
    zr(jcmxf+zcmxf*(izone-1)+13-1) = algofr
!
! --- CARACTERISTIQUES DU FROTTEMENT
!
    if (lfrot) then
        zr(jcmxf+zcmxf*(izone-1)+5-1) = 3.d0
        call getvr8(motfac, 'COULOMB', iocc=izone, scal=coefff, nbret=noc)
        zr(jcmxf+zcmxf*(izone-1)+4-1) = coefff
        call getvr8(motfac, 'SEUIL_INIT', iocc=izone, scal=reacsi, nbret=noc)
        zr(jcmxf+zcmxf*(izone-1)+6-1) = reacsi
    else
        zr(jcmxf+zcmxf*(izone-1)+5-1) = 1.d0
    endif
!
! --- CONTACT INITIAL
!
    if (algocr .ne. 3.d0) then
        call getvtx(motfac, 'CONTACT_INIT', iocc=izone, scal=staco0, nbret=noc)
        if (staco0 .eq. 'OUI') then
            zr(jcmxf+zcmxf*(izone-1)+7-1) = 1.d0
        else if (staco0 .eq. 'NON') then
            zr(jcmxf+zcmxf*(izone-1)+7-1) = 0.d0
        else
            ASSERT(.false.)
        endif
!
! --- GLISSIERE
!
        call getvtx(motfac, 'GLISSIERE', iocc=izone, scal=glis, nbret=noc)
        if (glis(1:3) .eq. 'OUI') then
            zr(jcmxf+zcmxf*(izone-1)+10-1) = 1.d0
        else if (glis(1:3) .eq. 'NON') then
            zr(jcmxf+zcmxf*(izone-1)+10-1) = 0.d0
        else
            ASSERT(.false.)
        endif
    endif
!
! --- ALGORITHME DE RESTRICTION DE L'ESPACE DES MULITPLICATEURS
!
    call getvtx(motfac, 'ALGO_LAGR', iocc=1, scal=algola, nbret=noc)
    if (algola .eq. 'NON') then
        zr(jcmxf+zcmxf*(izone-1)+9-1) = 0.d0
    else if (algola.eq.'VERSION1') then
        zr(jcmxf+zcmxf*(izone-1)+9-1) = 1.d0
    else if (algola.eq.'VERSION2') then
        zr(jcmxf+zcmxf*(izone-1)+9-1) = 2.d0
    else
        ASSERT(.false.)
    endif
!
! --- PARAMETRE APPARIEMENT: TOLE_PROJ_EXT
! --- TOLE_PROJ_EXT <0: LA PROJECTION HORS DE LA MAILLE EST INTERDITE
! --- TOLE_PROJ_EXT >0: LA PROJECTION HORS DE LA MAILLE EST AUTORISEE
! ---                    MAIS LIMITEE PAR TOLJ
!
    call getvr8(motfac, 'TOLE_PROJ_EXT', iocc=izone, scal=tolj, nbret=noc)
    if (tolj .lt. 0.d0) then
        zr(jcmxf+zcmxf*(izone-1)+15-1) = -1.d0
    else
        zr(jcmxf+zcmxf*(izone-1)+15-1) = tolj
    endif
!
! --- RELATION CZM-XFEM
!
    if (algocr .eq. 3.d0) then
        call getvtx(motfac, 'RELATION', iocc=izone, scal=rela, nbret=noc)
        if (rela .eq. 'CZM_EXP_REG') then
            zr(jcmxf+zcmxf*(izone-1)+16-1) = 1.d0
        else if (rela.eq.'CZM_LIN_REG') then
            zr(jcmxf+zcmxf*(izone-1)+16-1) = 2.d0
        else if (rela .eq. 'CZM_TAC_MIX') then
            zr(jcmxf+zcmxf*(izone-1)+16-1) = 3.d0
        else if (rela.eq.'CZM_OUV_MIX') then
            zr(jcmxf+zcmxf*(izone-1)+16-1) = 4.d0
        else
            zr(jcmxf+zcmxf*(izone-1)+16-1) = 0.d0
        endif
    endif
!
    call jedema()
end subroutine
