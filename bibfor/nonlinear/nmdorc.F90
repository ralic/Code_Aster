subroutine nmdorc(modelz, compoz, carcri)
!
! person_in_charge: jean-michel.proix at edf.fr
!
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmdocc.h"
#include "asterfort/nmdocr.h"
    character(len=*) :: modelz, compoz
    character(len=24) :: carcri
!
! ----------------------------------------------------------------------
!
! SAISIE ET VERIFICATION DES MOTS CLES COMPORTEMENT / COMP_ELAS
!
! ----------------------------------------------------------------------
!
! IN  MODELZ : NOM DU MODELE
! OUT COMPOZ : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
! OUT CARCRI : CARTE DE CRITERES LOCAUX
!
! ----------------------------------------------------------------------
!
    integer :: ncmpma, dimaki, nbmo1, iret, dimanv
!    DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT
    parameter (dimaki=9)
!    DIMANV = DIMENSION MAX DE LA LISTE DU NOMBRE DE VAR INT EN THM
    parameter (dimanv=4)
    parameter (ncmpma=7+dimaki+dimanv)
    character(len=8) :: nomcmp(ncmpma), k8b
    character(len=16) :: moclef, k16bid, nomcmd
    character(len=19) :: compor
    character(len=24) :: modele
    logical :: criloc, meca
!
    data nomcmp/'RELCOM  ','NBVARI  ','DEFORM  ','INCELA  ',&
     &     'C_PLAN  ','XXXX1','XXXX2','KIT1    ','KIT2    ','KIT3    ',&
     &     'KIT4    ','KIT5    ','KIT6    ','KIT7    ','KIT8    ',&
     &     'KIT9    ', 'NVI_C   ', 'NVI_T   ', 'NVI_H   ', 'NVI_M   '/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    criloc = .false.
    meca = .false.
    modele = modelz
    compor = '&&NMDORC.COMPOR'
!
    call getres(k8b, k16bid, nomcmd)
!
!     MOCLEF= MOT CLE FACTEUR COMPORTEMENT  
!     CRILOC = EXISTENCE DE CRITERES LOCAUX DE CONVERGENCE
!     MECA=COMMANDES MECANIQUE
!
    moclef = 'COMPORTEMENT    '
    nbmo1=1
    if (nomcmd(1:13) .eq. 'THER_NON_LINE') then
        meca=.false.
    else if (nomcmd(1:9).eq.'LIRE_RESU' .or. nomcmd(1:9).eq.'CREA_RESU') then
        meca=.true.
    else if (nomcmd.eq.'CALC_FORC_NONL') then
        meca=.true.
        criloc=.false.
    else if (nomcmd(1:6) .eq.'CALC_G') then
        meca=.true.
        elseif ((nomcmd(1:13).eq.'STAT_NON_LINE').or. (nomcmd(1:13)&
    .eq.'DYNA_NON_LINE').or. (nomcmd(1:6) .eq.'CALCUL') ) then
        meca=.true.
        criloc=.true.
    else
        ASSERT(.false.)
    endif
!
! --- CARTE COMPOR
!
    call nmdocc(compor, modele, nbmo1, moclef, nomcmp,&
                ncmpma, meca, nomcmd)
    compoz = compor
!
! --- CARTE DE CRITERES LOCAUX
!
    if (criloc) then
        call nmdocr(carcri, modele, nbmo1, moclef, iret)
    endif
!
    call jedema()
end subroutine
