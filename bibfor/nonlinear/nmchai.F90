subroutine nmchai(tychap, tyvarz, vali)
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
#include "asterc/indik8.h"
#include "asterfort/assert.h"
    character(len=6) :: tychap
    character(len=*) :: tyvarz
    integer :: vali
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! INDEX OU EST STOCKE LE NOM DE LA VARIABLE DANS UNE VARIABLE CHAPEAU
!
! ----------------------------------------------------------------------
!
!
! IN  TYCHAP : TYPE DE VARIABLE CHAPEAU
!                MEELEM - NOMS DES MATR_ELEM
!                MEASSE - NOMS DES MATR_ASSE
!                VEELEM - NOMS DES VECT_ELEM
!                VEASSE - NOMS DES VECT_ASSE
!                SOLALG - NOMS DES CHAM_NO SOLUTIONS
!                VALINC - VALEURS SOLUTION INCREMENTALE
! IN  TYVARI : TYPE DE LA VARIABLE
!                OU  LONUTI - NOMBRE DE VAR. STOCKEES
! OUT VALI   : INDEX OU EST STOCKE LE NOM DE LA VARIABLE DANS UNE
!              VARIABLE CHAPEAU
!                OU NOMBRE DE VAR. STOCKEES
!
! ----------------------------------------------------------------------
!
    integer :: zmeelm, zmeass, zveelm, zveass
    parameter    (zmeelm=9 ,zmeass=4 ,zveelm=21,zveass=32)
    integer :: zsolal, zvalin
    parameter    (zsolal=17,zvalin=28)
!
    character(len=8) :: lmeelm(zmeelm), lmeass(zmeass)
    character(len=8) :: lveelm(zveelm), lveass(zveass)
    character(len=8) :: lsolal(zsolal)
    character(len=8) :: lvalin(zvalin)
!
    character(len=8) :: tyvari
!
    data lmeelm /'MERIGI','MEDIRI','MEMASS','MEAMOR','MESUIV',&
     &             'MESSTR','MEGEOM','MEELTC','MEELTF'/
    data lmeass /'MERIGI','MEMASS','MEAMOR','MESSTR'/
!
    data lveelm /'CNFINT','CNDIRI','CNBUDI','CNFNOD','CNDIDO',&
     &             'CNDIPI','CNFEDO','CNFEPI','CNLAPL','CNONDP',&
     &             'CNFSDO','CNIMPP','      ','CNDIDI','CNSSTF',&
     &             'CNELTC','CNELTF','CNREFE','CNVCF1','CNVCF0',&
     &             'CNIMPC'/
    data lveass /'CNFINT','CNDIRI','CNBUDI','CNFNOD','CNDIDO',&
     &             'CNDIPI','CNFEDO','CNFEPI','CNLAPL','CNONDP',&
     &             'CNFSDO','CNIMPP','      ','CNDIDI','CNSSTF',&
     &             'CNELTC','CNELTF','CNREFE','CNVCF1','CNVCF0',&
     &             'CNCINE','CNSSTR','CNCTDF','CNVCPR','CNDYNA',&
     &             'CNMODP','CNMODC','CNCTDC','CNUNIL','CNFEXT',&
     &             'CNIMPC','CNVISS'/
!
    data lsolal /'DDEPLA','DEPDEL','DEPOLD','DEPPR1','DEPPR2',&
     &             'DVITLA','VITDEL','VITOLD','VITPR1','VITPR2',&
     &             'DACCLA','ACCDEL','ACCOLD','ACCPR1','ACCPR2',&
     &             'DEPSO1','DEPSO2'/
!
    data lvalin /'DEPMOI','SIGMOI','VARMOI','VITMOI','ACCMOI',&
     &             'COMMOI','DEPPLU','SIGPLU','VARPLU','VITPLU',&
     &             'ACCPLU','COMPLU','SIGEXT','DEPKM1','VITKM1',&
     &             'ACCKM1','ROMKM1','ROMK'  ,'STRMOI','STRPLU',&
     &             'FEXMOI','FEXPLU','FAMMOI','FAMPLU','FLIMOI',&
     &             'FLIPLU','FNOMOI','FNOPLU'/
!
! ----------------------------------------------------------------------
!
    vali = -1
    tyvari = tyvarz
!
! ---
!
    if (tychap .eq. 'MEELEM') then
        if (tyvari .eq. 'LONMAX') then
            vali = zmeelm
        else
            vali = indik8(lmeelm,tyvari,1,zmeelm)
        endif
    else if (tychap.eq.'MEASSE') then
        if (tyvari .eq. 'LONMAX') then
            vali = zmeass
        else
            vali = indik8(lmeass,tyvari,1,zmeass)
        endif
    else if (tychap.eq.'VEELEM') then
        if (tyvari .eq. 'LONMAX') then
            vali = zveelm
        else
            vali = indik8(lveelm,tyvari,1,zveelm)
        endif
    else if (tychap.eq.'VEASSE') then
        if (tyvari .eq. 'LONMAX') then
            vali = zveass
        else
            vali = indik8(lveass,tyvari,1,zveass)
        endif
    else if (tychap.eq.'SOLALG') then
        if (tyvari .eq. 'LONMAX') then
            vali = zsolal
        else
            vali = indik8(lsolal,tyvari,1,zsolal)
        endif
    else if (tychap.eq.'VALINC') then
        if (tyvari .eq. 'LONMAX') then
            vali = zvalin
        else
            vali = indik8(lvalin,tyvari,1,zvalin)
        endif
    else
        ASSERT(.false.)
    endif
!
    ASSERT(vali.gt.0)
!
end subroutine
