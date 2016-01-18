subroutine rscrsd(base, nomsd, typesd, nbordr)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/utpara.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: base, nomsd, typesd
    integer :: nbordr
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: jacques.pellet at edf.fr
! ----------------------------------------------------------------------
!      CREATION D'UNE STRUCTURE DE DONNEES "RESULTAT-COMPOSE".
!      (SI CETTE STRUCTURE EXISTE DEJA, ON LA DETRUIT).
!     ------------------------------------------------------------------
! IN  NOMSD  : NOM DE LA STRUCTURE "RESULTAT" A CREER.
! IN  TYPESD : TYPE DE LA STRUCTURE "RESULTAT" A CREER.
! IN  NBORDR : NOMBRE MAX DE NUM. D'ORDRE.
! ----------------------------------------------------------------------
    integer :: i, k, iret, jordr
    integer :: nbcham, nbnova
    integer :: ncmec1, ncmec2, ncmec3, ncmuti, ncmeca
    integer :: ncthe1, ncther, ncvarc, ncacou
    character(len=1) :: bas1
    character(len=16) :: types2
    character(len=19) :: noms2
!     ------------------------------------------------------------------
!                      C H A M P _ M E C A N I Q U E
!     ------------------------------------------------------------------
    parameter (ncmec1=35)
    parameter (ncmec2=49)
    parameter (ncmec3=34)
    parameter (ncmuti=40)
    parameter (ncmeca=ncmec1+ncmec2+ncmec3+ncmuti)
    character(len=16) :: chmec1(ncmec1)
    character(len=16) :: chmec2(ncmec2)
    character(len=16) :: chmec3(ncmec3)
    character(len=16) :: chmuti(ncmuti)
    character(len=16) :: chmeca(ncmeca)
!     ------------------------------------------------------------------
!                      C H A M P _ T H E R M I Q U E
!     ------------------------------------------------------------------
    parameter (ncthe1=17)
    parameter (ncther=ncthe1+ncmuti)
    character(len=16) :: chthe1(ncthe1)
    character(len=16) :: chther(ncther)
!     ------------------------------------------------------------------
!                      C H A M P _ V A R C
!     ------------------------------------------------------------------
    parameter (ncvarc=9)
    character(len=16) :: chvarc(ncvarc)
!     ------------------------------------------------------------------
!                      C H A M P _ A C O U S T I Q U E
!     ------------------------------------------------------------------
    parameter (ncacou=5)
    character(len=16) :: chacou(ncacou)
!     ------------------------------------------------------------------
!                      C H A M P _ M E C A N I Q U E
!     ------------------------------------------------------------------
!      '1234567890123456','1234567890123456','1234567890123456',
    data chmec1/&
     & 'DEPL',            'VITE',            'ACCE',&
     & 'DEPL_ABSOLU',     'VITE_ABSOLU',     'ACCE_ABSOLU',&
     & 'EFGE_ELNO',       'EFGE_NOEU',&
     & 'EPSI_ELGA',       'EPSI_ELNO',&
     & 'EPSI_NOEU',       'SIEF_ELGA',&
     & 'SIGM_ELGA',       'EFGE_ELGA',&
     & 'SIEF_ELNO',       'SIEF_NOEU',       'SIGM_ELNO',&
     & 'SIGM_NOEU',       'SIZ1_NOEU',       'SIZ2_NOEU',&
     & 'SIPO_ELNO',       'SIPO_NOEU',&
     & 'SIEQ_ELGA',       'SIEQ_ELNO',       'SIEQ_NOEU',&
     & 'EPEQ_ELGA',       'EPEQ_ELNO',       'EPEQ_NOEU',&
     & 'SIRO_ELEM',       'FLHN_ELGA',&
     & 'SIPM_ELNO',       'STRX_ELGA',       'FORC_EXTE',&
     & 'FORC_AMOR',       'FORC_LIAI'/
!
!      '1234567890123456','1234567890123456','1234567890123456',
    data chmec2/&
     & 'DEGE_ELNO',       'DEGE_NOEU',       'DEGE_ELGA',&
     & 'EPOT_ELEM',&
     & 'ECIN_ELEM',       'FORC_NODA',       'REAC_NODA',&
     & 'ERME_ELEM',       'ERME_ELNO',       'ERME_NOEU',&
     & 'ERZ1_ELEM',       'ERZ2_ELEM',       'QIRE_ELEM',&
     & 'QIRE_ELNO',       'QIRE_NOEU',       'QIZ1_ELEM',&
     & 'QIZ2_ELEM',       'EPSG_ELGA',       'EPSG_ELNO',&
     & 'EPSG_NOEU',       'EPSP_ELGA',       'EPSP_ELNO',&
     & 'EPSP_NOEU',       'VARI_ELGA',&
     & 'VARI_NOEU',       'VARI_ELNO',&
     & 'EPSA_ELNO',       'EPSA_NOEU',&
     & 'COMPORTEMENT',    'DERA_ELGA',       'DERA_ELNO',&
     & 'DERA_NOEU',       'PRME_ELNO',       'EPME_NOEU',&
     & 'EPME_ELNO',       'EPME_ELGA',       'EPMG_ELNO',&
     & 'EPMG_ELGA',       'ENEL_ELGA',       'ENEL_ELNO',&
     & 'ENEL_NOEU',       'ENEL_ELEM',&
     & 'EPMG_NOEU',       'SING_ELEM',       'SING_ELNO',&
     & 'DISS_ELGA',       'DISS_ELNO',       'DISS_NOEU',&
     & 'DISS_ELEM'/
!
!      '1234567890123456','1234567890123456','1234567890123456',
    data chmec3/&
     & 'EPMQ_ELGA',       'EPMQ_ELNO',       'EPMQ_NOEU',&
     & 'EPFP_ELNO',       'EPFP_ELGA',&
     & 'EPFD_ELNO',       'EPFD_ELGA',&
     & 'EPVC_ELNO',       'EPVC_ELGA',       'CONT_NOEU',&
     & 'ETOT_ELGA',       'ETOT_ELNO',       'ETOT_ELEM',&
     & 'MODE_FLAMB',      'ETOT_NOEU',&
     & 'ENDO_ELGA',       'ENDO_ELNO',       'ENDO_NOEU',&
     & 'INDL_ELGA',       'VAEX_ELGA',       'VAEX_ELNO',&
     & 'VAEX_NOEU',       'DEPL_VIBR',       'SISE_ELNO',&
     & 'COHE_ELEM',       'INDC_ELEM',       'SECO_ELEM',&
     & 'VARC_ELGA',       'FERRAILLAGE',     'EPVC_NOEU',&
     & 'EPFD_NOEU',       'EPFP_NOEU',       'PDIL_ELGA',&
     & 'MODE_STAB'/
!
!      '1234567890123456','1234567890123456','1234567890123456',
    data chmuti/&
     & 'UT01_ELGA',       'UT01_ELNO',      'UT01_ELEM', 'UT01_NOEU',&
     & 'UT02_ELGA',       'UT02_ELNO',      'UT02_ELEM', 'UT02_NOEU',&
     & 'UT03_ELGA',       'UT03_ELNO',      'UT03_ELEM', 'UT03_NOEU',&
     & 'UT04_ELGA',       'UT04_ELNO',      'UT04_ELEM', 'UT04_NOEU',&
     & 'UT05_ELGA',       'UT05_ELNO',      'UT05_ELEM', 'UT05_NOEU',&
     & 'UT06_ELGA',       'UT06_ELNO',      'UT06_ELEM', 'UT06_NOEU',&
     & 'UT07_ELGA',       'UT07_ELNO',      'UT07_ELEM', 'UT07_NOEU',&
     & 'UT08_ELGA',       'UT08_ELNO',      'UT08_ELEM', 'UT08_NOEU',&
     & 'UT09_ELGA',       'UT09_ELNO',      'UT09_ELEM', 'UT09_NOEU',&
     & 'UT10_ELGA',       'UT10_ELNO',      'UT10_ELEM', 'UT10_NOEU'/
!     ------------------------------------------------------------------
!                      C H A M P _ T H E R M I Q U E
!     ------------------------------------------------------------------
!      '1234567890123456','1234567890123456','1234567890123456',
    data chthe1/&
     & 'TEMP',&
     & 'FLUX_ELGA',       'FLUX_ELNO',       'FLUX_NOEU',&
     & 'META_ELNO',       'META_NOEU',&
     & 'DURT_ELNO',       'DURT_NOEU',       'ETHE_ELEM',&
     & 'HYDR_ELNO',       'HYDR_NOEU',&
     & 'SOUR_ELGA',       'COMPORTHER',&
     & 'ERTH_ELEM',       'ERTH_ELNO',       'ERTH_NOEU',&
     & 'TEMP_ELGA'/
!     ------------------------------------------------------------------
!                      C H A M P _ V A R C
!     ------------------------------------------------------------------
!      '1234567890123456','1234567890123456','1234567890123456',
    data chvarc/&
     & 'IRRA',            'TEMP',            'HYDR_ELNO',&
     & 'HYDR_NOEU',       'EPSA_ELNO',       'META_ELNO',&
     & 'PTOT',            'DIVU',            'NEUT'         /
!     ------------------------------------------------------------------
!                      C H A M P _ A C O U S T I Q U E
!     ------------------------------------------------------------------
!      '1234567890123456','1234567890123456','1234567890123456',
    data chacou/&
     & 'PRES',            'PRAC_ELNO',       'PRAC_NOEU',&
     & 'INTE_ELNO',       'INTE_NOEU'/
!     ------------------------------------------------------------------
!
    noms2=nomsd
    types2=typesd
    bas1=base
!
!     --- SI LA SD EXISTE DEJA, ON S'ARRETE EN ERREUR F :
    call jeexin(noms2//'.DESC', iret)
    ASSERT(iret.eq.0)
!
!     --- CREATION DE .DESC  ET  .ORDR ---
    call jecreo(noms2//'.DESC', bas1//' N K16')
    call wkvect(noms2//'.ORDR', bas1//' V I', nbordr, jordr)
    call jeecra(noms2//'.ORDR', 'LONUTI', 0)
!
    do i = 1, ncmec1
        chmeca(i)=chmec1(i)
    end do
    do i = 1, ncmec2
        chmeca(i+ncmec1)=chmec2(i)
    end do
    do i = 1, ncmec3
        chmeca(i+ncmec1+ncmec2)=chmec3(i)
    end do
    do i = 1, ncmuti
        chmeca(i+ncmec1+ncmec2+ncmec3)=chmuti(i)
    end do
!
    do i = 1, ncthe1
        chther(i)=chthe1(i)
    end do
    do i = 1, ncmuti
        chther(i+ncthe1)=chmuti(i)
    end do
!
!     -- DECLARATION ET INITIALISATION DES PARAMETRES ET VAR. D'ACCES :
!     ------------------------------------------------------------------
    call utpara(bas1, nomsd, types2, nbordr)
!
!     ------------------------------------------------------------------
    if (types2 .eq. 'EVOL_ELAS') then
!
        nbcham=ncmeca
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='EVEL')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chmeca(i)))
        enddo
!
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'MULT_ELAS') then
!
        nbcham=ncmeca
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='MUEL')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chmeca(i)))
        enddo
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'FOURIER_ELAS') then
!
        nbcham=ncmeca
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='FOEL')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chmeca(i)))
        enddo
!
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'FOURIER_THER') then
!
        nbcham=ncther
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='FOTH')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chther(i)))
        enddo
!
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'EVOL_NOLI') then
!
        nbcham=ncmeca
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='EVNO')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chmeca(i)))
        enddo
!
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'DYNA_TRANS') then
!
        nbcham=ncmeca
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='DYTR')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chmeca(i)))
        enddo
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'DYNA_HARMO') then
!
        nbcham=ncmeca
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='DYHA')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chmeca(i)))
        enddo
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'HARM_GENE') then
!
        nbcham=ncmeca
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='HAGE')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chmeca(i)))
        enddo
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'ACOU_HARMO') then
!
        nbcham=ncacou
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='ACHA')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chacou(i)))
        enddo
!
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'EVOL_CHAR') then
!
        nbcham=8
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='EVCH')
        call jecroc(jexnom(noms2//'.DESC', 'PRES'))
        call jecroc(jexnom(noms2//'.DESC', 'FVOL_3D'))
        call jecroc(jexnom(noms2//'.DESC', 'FVOL_2D'))
        call jecroc(jexnom(noms2//'.DESC', 'FSUR_3D'))
        call jecroc(jexnom(noms2//'.DESC', 'FSUR_2D'))
        call jecroc(jexnom(noms2//'.DESC', 'VITE_VENT'))
        call jecroc(jexnom(noms2//'.DESC', 'T_EXT'))
        call jecroc(jexnom(noms2//'.DESC', 'COEF_H'))

        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'EVOL_THER') then
!
        nbcham=ncther
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='EVTH')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chther(i)))
        enddo
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'EVOL_VARC') then
!
        nbcham=ncvarc
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='EVVA')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chvarc(i)))
        enddo
        goto 99
!
!     ------------------------------------------------------------------
        elseif (types2.eq.'MODE_MECA' .or. types2.eq.'MODE_MECA_C' .or.&
     &        types2.eq.'MODE_GENE' .or. types2.eq.'MODE_ACOU' .or.&
     &        types2.eq.'DYNAMIQUE' ) then
!
        if (types2 .eq. 'MODE_MECA') then
            call jeecra(noms2//'.DESC', 'DOCU', cval='MOME')
        else if (types2.eq.'MODE_MECA_C') then
            call jeecra(noms2//'.DESC', 'DOCU', cval='MOME')
        else if (types2.eq.'MODE_GENE') then
            call jeecra(noms2//'.DESC', 'DOCU', cval='MOGE')
        else if (types2.eq.'DYNAMIQUE') then
            call jeecra(noms2//'.DESC', 'DOCU', cval='BAMO')
        else if (types2.eq.'MODE_ACOU') then
            call jeecra(noms2//'.DESC', 'DOCU', cval='MOAC')
        endif
!
        if (types2 .eq. 'MODE_ACOU') then
            nbcham=1
            call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
            call jecroc(jexnom(noms2//'.DESC', 'PRES'))
        else
            nbcham=ncmeca
            call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
            do i = 1, nbcham
                call jecroc(jexnom(noms2//'.DESC', chmeca(i)))
            enddo
        endif
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'MODE_FLAMB') then
!
        nbcham=ncmeca
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='MOFL')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chmeca(i)))
        enddo
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'MODE_STAB') then
!
        nbcham=ncmeca
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='MOSB')
        do i = 1, nbcham
            call jecroc(jexnom(noms2//'.DESC', chmeca(i)))
        enddo
        goto 99
!
!     ------------------------------------------------------------------
    else if (types2.eq.'COMB_FOURIER') then
!
        nbcham=ncmeca+ncthe1
        call jeecra(noms2//'.DESC', 'NOMMAX', nbcham)
        call jeecra(noms2//'.DESC', 'DOCU', cval='COFO')
        do i = 1, ncmeca
            call jecroc(jexnom(noms2//'.DESC', chmeca(i)))
        enddo
        do i = 1, ncthe1
            call jecroc(jexnom(noms2//'.DESC', chthe1(i)))
        enddo
        goto 99

    else
        call utmess('F', 'UTILITAI4_31', sk=types2)
    endif
!
!     ------------------------------------------------------------------
99  continue
!
!     --- CREATION DE .TACH
!     -------------------------
    call jecrec(noms2//'.TACH', bas1//' V K24', 'NU', 'CONTIG', 'CONSTANT',&
                nbcham)
    call jeecra(noms2//'.TACH', 'LONMAX', nbordr)
!
!
!     -- POUR QUE LES COLLECTIONS .TACH ET .TAVA SOIENT BIEN CREEES :
!     ---------------------------------------------------------------
    do k = 1, nbcham
        call jecroc(jexnum(noms2//'.TACH', k))
    end do
    call jelira(noms2//'.NOVA', 'NOMMAX', nbnova)
    do k = 1, nbnova
        call jecroc(jexnum(noms2//'.TAVA', k))
    end do
!
end subroutine
