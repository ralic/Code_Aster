subroutine nmch5p(veasse)
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
    character(len=19) :: veasse(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (INITIALISATION)
!
! CREATION DES VARIABLES CHAPEAUX - VEASSE
!
! ----------------------------------------------------------------------
!
!
! OUT VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
!
! ----------------------------------------------------------------------
!
    character(len=19) :: cnfedo, cnfepi, cnfsdo, cndidi, cnfint
    character(len=19) :: cndido, cndipi, cncine, cndiri, cnfnod
    character(len=19) :: cnbudi, cnlapl, cnsstr, cnctdf, cnondp
    character(len=19) :: cneltc, cneltf
    character(len=19) :: cnsstf
    character(len=19) :: cnrefe, cnvcf0, cnvcf1, cnvcpr
    character(len=19) :: cndyna, cnmodp, cnmodc
    character(len=19) :: cnctdc, cnunil, cnfext
    character(len=19) :: cnimpp, cnimpc, cnviss
!
    data cnfedo,cnfsdo    /'&&NMCH5P.CNFEDO','&&NMCH5P.CNFSDO'/
    data cndido,cnfepi    /'&&NMCH5P.CNDIDO','&&NMCH5P.CNFEPI'/
    data cndipi,cnfint    /'&&NMCH5P.CNDIPI','&&NMCH5P.CNFINT'/
    data cnbudi,cndidi    /'&&NMCH5P.CNBUDI','&&NMCH5P.CNDIDI'/
    data cnondp,cnlapl    /'&&NMCH5P.CNONDP','&&NMCH5P.CNLAPL'/
    data cndiri,cnfnod    /'&&NMCH5P.CNDIRI','&&NMCH5P.CNFNOD'/
    data cnsstf,cneltc    /'&&NMCH5P.CNSSTF','&&NMCH5P.CNELTC'/
    data cneltf           /'&&NMCH5P.CNELTF'/
    data cnrefe           /'&&NMCH5P.CNREFE'/
    data cnvcf0,cnvcf1    /'&&NMCH5P.CNVCF0','&&NMCH5P.CNVCF1'/
    data cncine,cnsstr    /'&&NMCH5P.CNCINE','&&NMCH5P.CNSSTR'/
    data cnctdf           /'&&NMCH5P.CNCTDF'/
    data cnvcpr,cnmodp    /'&&NMCH5P.CNVCPR','&&NMCH5P.CNMODP'/
    data cndyna           /'&&NMCH5P.CNDYNA'/
    data cnmodc,cnctdc    /'&&NMCH5P.CNMODC','&&NMCH5P.CNCTDC'/
    data cnunil,cnfext    /'&&NMCH5P.CNUNIL','&&NMCH5P.CNFEXT'/
    data cnimpp,cnimpc    /'&&NMCH5P.CNIMPP','&&NMCH5P.CNIMPC'/
    data cnviss           /'&&NMCH5P.CNVISS'/
!
! ----------------------------------------------------------------------
!
    call nmcha0('VEASSE', 'ALLINI', ' ', veasse)
    call nmcha0('VEASSE', 'CNFINT', cnfint, veasse)
    call nmcha0('VEASSE', 'CNDIRI', cndiri, veasse)
    call nmcha0('VEASSE', 'CNBUDI', cnbudi, veasse)
    call nmcha0('VEASSE', 'CNFNOD', cnfnod, veasse)
    call nmcha0('VEASSE', 'CNDIDO', cndido, veasse)
    call nmcha0('VEASSE', 'CNDIPI', cndipi, veasse)
    call nmcha0('VEASSE', 'CNFEDO', cnfedo, veasse)
    call nmcha0('VEASSE', 'CNFEPI', cnfepi, veasse)
    call nmcha0('VEASSE', 'CNLAPL', cnlapl, veasse)
    call nmcha0('VEASSE', 'CNONDP', cnondp, veasse)
    call nmcha0('VEASSE', 'CNFSDO', cnfsdo, veasse)
    call nmcha0('VEASSE', 'CNIMPP', cnimpp, veasse)
    call nmcha0('VEASSE', 'CNIMPC', cnimpc, veasse)
    call nmcha0('VEASSE', 'CNDIDI', cndidi, veasse)
    call nmcha0('VEASSE', 'CNSSTF', cnsstf, veasse)
    call nmcha0('VEASSE', 'CNELTC', cneltc, veasse)
    call nmcha0('VEASSE', 'CNELTF', cneltf, veasse)
    call nmcha0('VEASSE', 'CNREFE', cnrefe, veasse)
    call nmcha0('VEASSE', 'CNVCF1', cnvcf1, veasse)
    call nmcha0('VEASSE', 'CNVCF0', cnvcf0, veasse)
!
! --- SANS VECT_ELEM POUR CONSTRUIRE
!
    call nmcha0('VEASSE', 'CNCINE', cncine, veasse)
    call nmcha0('VEASSE', 'CNSSTR', cnsstr, veasse)
    call nmcha0('VEASSE', 'CNCTDF', cnctdf, veasse)
    call nmcha0('VEASSE', 'CNVCPR', cnvcpr, veasse)
    call nmcha0('VEASSE', 'CNDYNA', cndyna, veasse)
    call nmcha0('VEASSE', 'CNMODP', cnmodp, veasse)
    call nmcha0('VEASSE', 'CNMODC', cnmodc, veasse)
    call nmcha0('VEASSE', 'CNCTDC', cnctdc, veasse)
    call nmcha0('VEASSE', 'CNUNIL', cnunil, veasse)
    call nmcha0('VEASSE', 'CNFEXT', cnfext, veasse)
    call nmcha0('VEASSE', 'CNVISS', cnviss, veasse)
!
end subroutine
