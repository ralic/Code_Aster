subroutine nmch4p(veelem)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "asterfort/nmcha0.h"
    character(len=19) :: veelem(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! CREATION DES VARIABLES CHAPEAUX - VEELEM
!
! ----------------------------------------------------------------------
!
! OUT VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
!
! ----------------------------------------------------------------------
!
    character(len=19) :: vefsdo, vefint, vebudi, vedido, vesstf
    character(len=19) :: vedipi, vefedo, vefepi, veondp
    character(len=19) :: vedidi, vediri, vefnod, velapl
    character(len=19) :: veeltc, veeltf
    character(len=19) :: verefe, vevcf0, vevcf1
    character(len=19) :: veimpp, veimpc
!
    data vefedo,vefsdo    /'&&NMCH4P.VEFEDO','&&NMCH4P.VEFSDO'/
    data vedido,vefepi    /'&&NMCH4P.VEDIDO','&&NMCH4P.VEFEPI'/
    data vedipi,vefint    /'&&NMCH4P.VEDIPI','&&NMCH4P.VEFINT'/
    data vebudi,vedidi    /'&&NMCH4P.VEBUDI','&&NMCH4P.VEDIDI'/
    data veondp,velapl    /'&&NMCH4P.VEONDP','&&NMCH4P.VELAPL'/
    data vediri,vefnod    /'&&NMCH4P.VEDIRI','&&NMCH4P.VEFNOD'/
    data vesstf,veeltc    /'&&NMCH4P.VESSTF','&&NMCH4P.VEELTC'/
    data veeltf           /'&&NMCH4P.VEELTF'/
    data verefe           /'&&NMCH4P.VEREFE'/
    data vevcf0,vevcf1    /'&&NMCH4P.VEVCF0','&&NMCH4P.VEVCF1'/
    data veimpp,veimpc    /'&&NMCH4P.VEIMPP','&&NMCH4P.VEIMPC'/
!
! ----------------------------------------------------------------------
!
    call nmcha0('VEELEM', 'ALLINI', ' ', veelem)
    call nmcha0('VEELEM', 'CNFINT', vefint, veelem)
    call nmcha0('VEELEM', 'CNDIRI', vediri, veelem)
    call nmcha0('VEELEM', 'CNBUDI', vebudi, veelem)
    call nmcha0('VEELEM', 'CNFNOD', vefnod, veelem)
    call nmcha0('VEELEM', 'CNDIDO', vedido, veelem)
    call nmcha0('VEELEM', 'CNDIPI', vedipi, veelem)
    call nmcha0('VEELEM', 'CNFEDO', vefedo, veelem)
    call nmcha0('VEELEM', 'CNFEPI', vefepi, veelem)
    call nmcha0('VEELEM', 'CNLAPL', velapl, veelem)
    call nmcha0('VEELEM', 'CNONDP', veondp, veelem)
    call nmcha0('VEELEM', 'CNFSDO', vefsdo, veelem)
    call nmcha0('VEELEM', 'CNIMPP', veimpp, veelem)
    call nmcha0('VEELEM', 'CNIMPC', veimpc, veelem)
    call nmcha0('VEELEM', 'CNDIDI', vedidi, veelem)
    call nmcha0('VEELEM', 'CNSSTF', vesstf, veelem)
    call nmcha0('VEELEM', 'CNELTC', veeltc, veelem)
    call nmcha0('VEELEM', 'CNELTF', veeltf, veelem)
    call nmcha0('VEELEM', 'CNREFE', verefe, veelem)
    call nmcha0('VEELEM', 'CNVCF1', vevcf1, veelem)
    call nmcha0('VEELEM', 'CNVCF0', vevcf0, veelem)
!
end subroutine
