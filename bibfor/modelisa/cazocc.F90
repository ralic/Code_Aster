subroutine cazocc(char, motfac, izone)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mminfl.h"
#include "asterfort/utmess.h"
    character(len=8) :: char
    character(len=16) :: motfac
    integer :: izone
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - LECTURE DONNEES)
!
! LECTURE DES PRINCIPALES CARACTERISTIQUES DU CONTACT (SURFACE IZONE)
! REMPLISSAGE DE LA SD 'DEFICO' (SURFACE IZONE)
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'ZONE')
! IN  IZONE  : INDICE POUR LIRE LES DONNEES DANS AFFE_CHAR_MECA
!
! ----------------------------------------------------------------------
!
    integer :: zcmcf, zexcl
    character(len=24) :: defico
    integer :: noc, nocc
    character(len=24) :: caracf, exclfr, sgrno
    integer :: jcmcf, jexclf
    character(len=16) :: glis, integ, staco0, algoc, algof
    real(kind=8) :: rexclf, direxf(3)
    real(kind=8) :: coefff, seuili
    real(kind=8) :: coefaf, coefac
    real(kind=8) :: algocr, algofr
    real(kind=8) :: typint, ctrini
    integer :: parint
    logical :: lintno, lfrot, lsscon, lssfro, lexdir
    logical :: lgliss, lnewtg, lnewtc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    lfrot = cfdisl(defico,'FROTTEMENT')
    lnewtg = cfdisl(defico,'GEOM_NEWTON')
    lnewtc = cfdisl(defico,'CONT_NEWTON')
!
    parint = 0
    typint = 0.d0
    algocr = 0.d0
    coefac = 100.d0
    algofr = 0.d0
    coefff = 0.d0
    coefaf = 100.d0
    seuili = 0.d0
    ctrini = 0.d0
    lintno = .false.
    lsscon = .false.
    lssfro = .false.
    rexclf = 0.d0
    direxf(1) = 0.d0
    direxf(2) = 0.d0
    direxf(3) = 0.d0
    lgliss = .false.
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    caracf = defico(1:16)//'.CARACF'
    exclfr = defico(1:16)//'.EXCLFR'
!
    call jeveuo(caracf, 'E', jcmcf)
    call jeveuo(exclfr, 'E', jexclf)
!
    zcmcf = cfmmvd('ZCMCF')
    zexcl = cfmmvd('ZEXCL')
!
! --- TYPE INTEGRATION
!
    call getvtx(motfac, 'INTEGRATION', iocc=izone, scal=integ, nbret=noc)
    if (integ(1:4) .eq. 'AUTO') then
        lintno = .true.
        typint = 1.d0
    else if (integ(1:5) .eq. 'GAUSS') then
        call getvis(motfac, 'ORDRE_INT', iocc=izone, scal=parint, nbret=noc)
        typint = 10.d0*parint + 2.d0
    else if (integ(1:7) .eq. 'SIMPSON') then
        call getvis(motfac, 'ORDRE_INT', iocc=izone, scal=parint, nbret=noc)
        typint = 10.d0*parint + 3.d0
    else if (integ(1:6) .eq. 'NCOTES') then
        call getvis(motfac, 'ORDRE_INT', iocc=izone, scal=parint, nbret=noc)
        typint = 10.d0*parint + 4.d0
    else
        ASSERT(.false.)
    endif
!
! --- OPTIONS CONTACT
!
    call getvtx(motfac, 'ALGO_CONT', iocc=izone, scal=algoc, nbret=noc)
    if (algoc(1:10) .eq. 'STANDARD') then
        call getvr8(motfac, 'COEF_CONT', iocc=izone, scal=coefac, nbret=noc)
        algocr = 1.d0
    else if (algoc(1:14) .eq. 'PENALISATION') then
        call getvr8(motfac, 'COEF_PENA_CONT', iocc=izone, scal=coefac, nbret=noc)
        algocr = 3.d0
    else
        ASSERT(.false.)
    endif
!
! --- OPTIONS FROTTEMENT
!
    if (lfrot) then
        call getvtx(motfac, 'ALGO_FROT', iocc=izone, scal=algof, nbret=noc)
        if (algof(1:10) .eq. 'STANDARD') then
            call getvr8(motfac, 'COEF_FROT', iocc=izone, scal=coefaf, nbret=noc)
            algofr = 1.d0
        else if (algof(1:14) .eq. 'PENALISATION') then
            call getvr8(motfac, 'COEF_PENA_FROT', iocc=izone, scal=coefaf, nbret=noc)
            algofr = 3.d0
        else
            ASSERT(.false.)
        endif
        if (algoc .ne. algof) then
            call utmess('F', 'CONTACT_89')
        endif
    else
        coefaf = 0.d0
        algofr = 0.d0
    endif
!
! --- INCOMPATIBILITES
!
    if (lfrot .and. (algoc.ne.algof)) then
        call utmess('F', 'CONTACT_89')
    endif
    if ((algoc.eq.'PENALISATION') .and. lnewtg) then
        call utmess('F', 'CONTACT_21')
    endif
    if (lnewtg .and. (.not.lnewtc)) then
        call utmess('F', 'CONTACT_20')
    endif
!
! --- CARACTERISTIQUES DU FROTTEMENT PAR ZONE
!
    if (lfrot) then
        call getvr8(motfac, 'COULOMB', iocc=izone, scal=coefff, nbret=noc)
        call getvr8(motfac, 'SEUIL_INIT', iocc=izone, scal=seuili, nbret=noc)
        if (coefff .eq. 0.d0) then
            coefaf = 0.d0
            algofr = 0.d0
        endif
    endif
!
! --- TRAITEMENT EXCLUSION NOEUDS CONTACT
!
    call getvtx(motfac, 'SANS_GROUP_NO', iocc=izone, scal=sgrno, nbret=noc)
    call getvtx(motfac, 'SANS_NOEUD', iocc=izone, scal=sgrno, nbret=nocc)
    lsscon = (noc.ne.0) .or. (nocc.ne.0)
!
    call getvtx(motfac, 'SANS_GROUP_MA', iocc=izone, scal=sgrno, nbret=noc)
    call getvtx(motfac, 'SANS_MAILLE', iocc=izone, scal=sgrno, nbret=nocc)
    lsscon = lsscon.or.((noc.ne.0).or.(nocc.ne.0))
!
! --- TRAITEMENT EXCLUSION NOEUDS FROTTEMENT
!
    call getvtx(motfac, 'SANS_GROUP_NO_FR', iocc=izone, scal=sgrno, nbret=noc)
    call getvtx(motfac, 'SANS_NOEUD_FR', iocc=izone, scal=sgrno, nbret=nocc)
    lssfro = (noc.ne.0) .or. (nocc.ne.0)
!
! --- SI NOEUD EXCLUS, ON VERIFIE QU'ON A UNE INTEGRATION AUX NOEUDS
!
    if (.not.lintno) then
        if (lsscon .or. lssfro) then
            call utmess('F', 'CONTACT_97')
        endif
        if (.not.mminfl(defico,'MAIT',izone )) then
            call utmess('F', 'CONTACT_98')
        endif
    endif
!
! --- NOMBRE DE DIRECTIONS A EXCLURE ET VECTEUR DIRECTEUR
!
    if (lssfro) then
        call getvr8(motfac, 'DIRE_EXCL_FROT', iocc=izone, nbval=3, vect=direxf,&
                    nbret=noc)
        lexdir = (noc .ne. 0)
        if (.not.lexdir) then
! ------- TOUTES LES DIRECTIONS SONT EXCLUES
            rexclf = 2.d0
            direxf(1) = 0.d0
            direxf(2) = 0.d0
            direxf(3) = 0.d0
        else
! ------- UNE SEULE DIRECTION EST EXCLUE
            rexclf = 1.d0
        endif
    else
        rexclf = 0.d0
        direxf(1) = 0.d0
        direxf(2) = 0.d0
        direxf(3) = 0.d0
    endif
!
! --- CONTACT INITIAL
!
    call getvtx(motfac, 'CONTACT_INIT', iocc=izone, scal=staco0, nbret=noc)
    if (staco0 .eq. 'OUI') then
        ctrini = 1.d0
    else if (staco0 .eq. 'INTERPENETRE') then
        ctrini = 2.d0
    else if (staco0 .eq. 'NON') then
        ctrini = 0.d0
    else
        ASSERT(.false.)
    endif
!
! --- GLISSIERE
!
    call getvtx(motfac, 'GLISSIERE', iocc=izone, scal=glis, nbret=noc)
    if (glis(1:3) .eq. 'OUI') then
        lgliss = .true.
    else if (glis(1:3) .eq. 'NON') then
        lgliss = .false.
    else
        ASSERT(.false.)
    endif
!
    zr(jcmcf-1+zcmcf*(izone-1)+1) = typint
    zr(jcmcf-1+zcmcf*(izone-1)+2) = coefac
    zr(jcmcf-1+zcmcf*(izone-1)+3) = algocr
    zr(jcmcf-1+zcmcf*(izone-1)+4) = coefaf
    zr(jcmcf-1+zcmcf*(izone-1)+5) = algofr
    zr(jcmcf-1+zcmcf*(izone-1)+6) = coefff
    zr(jcmcf-1+zcmcf*(izone-1)+7) = seuili
    zr(jcmcf-1+zcmcf*(izone-1)+8) = ctrini
    if (lgliss) then
        zr(jcmcf-1+zcmcf*(izone-1)+9) = 1.d0
    else
        zr(jcmcf-1+zcmcf*(izone-1)+9) = 0.d0
    endif
    if (lsscon) then
        zr(jcmcf-1+zcmcf*(izone-1)+10) = 1.d0
    else
        zr(jcmcf-1+zcmcf*(izone-1)+10) = 0.d0
    endif
    if (lssfro) then
        zr(jcmcf-1+zcmcf*(izone-1)+11) = 1.d0
    else
        zr(jcmcf-1+zcmcf*(izone-1)+11) = 0.d0
    endif
    zr(jcmcf-1+zcmcf*(izone-1)+12) = rexclf
    zr(jexclf-1+zexcl*(izone-1)+1) = direxf(1)
    zr(jexclf-1+zexcl*(izone-1)+2) = direxf(2)
    zr(jexclf-1+zexcl*(izone-1)+3) = direxf(3)
!
    call jedema()
end subroutine
