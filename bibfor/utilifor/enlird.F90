subroutine enlird(dateur)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ......................................................................
    implicit none
!   - FONCTION REALISEE:
!       ECRITURE DE LA D.A.T.E D'AUJOURD'HUI SUR LA VARIABLE DATEUR
!   - OUT :
!       DATEUR : CHARACTER*24
!   - AUTEUR : PRIS A SIVA POUR ASTER
! ......................................................................
    include 'asterc/kloklo.h'
    integer :: i, date9(9)
    character(len=2) :: jour2(0:6), date2(2:7)
    character(len=4) :: mois4(12), annee
    character(len=*) :: dateur
    character(len=24) :: dateuz
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data jour2/'LU','MA','ME','JE','VE','SA','DI'/
    data mois4/'JANV','FEVR','MARS','AVRI','MAI ','JUIN',&
     &           'JUIL','AOUT','SEPT','OCTO','NOVE','DECE'/
!     APPEL A LA GENERALE
    call kloklo(date9)
    date2(2) = '00'
    if (date9(2) .lt. 10) then
        write(date2(2)(2:2),'(I1)') date9(2)
    else
        write(date2(2)(1:2),'(I2)') date9(2)
    endif
    write(annee,'(I4)') date9(4)
    do 1 i = 5, 7
        date2(i) = '00'
        if (date9(i) .lt. 10) then
            write(date2(i)(2:2),'(I1)') date9(i)
        else
            write(date2(i)(1:2),'(I2)') date9(i)
        endif
 1  end do
    write (dateuz,101) jour2(date9(1)),date2(2),&
     &      mois4(date9(3)),annee,(date2(i),i=5,7)
    dateur = dateuz
    101 format(a2,'-',a2,'-',a4,'-',a4,1x,a2,':',a2,':',a2)
end subroutine
