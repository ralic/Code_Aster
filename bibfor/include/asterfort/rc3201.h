!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
!
interface
    subroutine rc3201(ze200, ig, lpmpb, lsn, lther, lfat, lefat,&
                      yapass, seisme, iocs, mater,&
                      lieu, utot, utotenv, resuas,&
                      resuss, resuca, resucs,&
                      factus, resumax)
        aster_logical :: ze200
        integer :: ig
        aster_logical :: lpmpb
        aster_logical :: lsn
        aster_logical :: lther
        aster_logical :: lfat
        aster_logical :: lefat
        aster_logical :: yapass
        aster_logical :: seisme
        integer :: iocs
        character(len=8) :: mater
        character(len=4) :: lieu
        real(kind=8) :: utot
        real(kind=8) :: utotenv
        real(kind=8) :: resuas(*)
        real(kind=8) :: resuss(*)
        real(kind=8) :: resuca(*)
        real(kind=8) :: resucs(*)
        real(kind=8) :: factus(*)
        real(kind=8) :: resumax(*)
    end subroutine rc3201
end interface
