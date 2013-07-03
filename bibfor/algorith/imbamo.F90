subroutine imbamo(nomres)
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
!***********************************************************************
!    P. RICHARD     DATE 21/02/1991
!-----------------------------------------------------------------------
!  BUT:  IMPRIMER LES RESULTATS RELATIFS A LA BASE MODALE
    implicit none
!
!-----------------------------------------------------------------------
!
! NOMRES   /I/: NOM DU CONCEPT RESULTAT
!
!
#include "jeveux.h"
!
!-----------------------------------------------------------------------
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: i, ibid, iret, nbdef, nbmod, nbpabm, nbtot
!
    real(kind=8) :: freq, genek, genem
!-----------------------------------------------------------------------
    parameter    (nbpabm=8)
    character(len=8) :: nomres, intf, nomnoe, nomcmp
    character(len=19) :: raid, mass, typeba, valk(4)
    character(len=14) :: numref
    integer :: ldpar(nbpabm), llref, ier, vali(2)
    character(len=16) :: bmpara(nbpabm), typdef
    character(len=8) :: rescyc
    character(len=8) :: k8bid
    real(kind=8) :: valr(3)
!
!-----------------------------------------------------------------------
!
    data  bmpara/&
     &  'NUME_MODE  '     , 'FREQ'       , 'NORME'           ,&
     &  'NOEUD_CMP'       , 'TYPE_DEFO'          , 'OMEGA2'   ,&
     &  'MASS_GENE'      , 'RIGI_GENE'/
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
!----------------DETERMINATION DU TYPE DE LA BASE-----------------------
!
!------------------RECUPERATION DES CONCEPT AMONT-----------------------
!
    call jeveuo(nomres//'           .REFD', 'L', llref)
    raid=zk24(llref)(1:19)
    mass=zk24(llref+1)(1:19)
    numref=zk24(llref+3)(1:14)
    intf=zk24(llref+4)(1:8)
    typeba=zk24(llref+6)(1:19)
!
!--------------------------------ECRITURES------------------------------
!
    call u2mesk('I', 'ALGELINE6_1', 1, nomres)
!
!    CAS D'UNE BASE DE TYPE CONNUE
!
    if (typeba(1:9) .eq. 'CLASSIQUE') then
!
        call dismoi('F', 'NB_MODES_TOT', nomres, 'RESULTAT', nbtot,&
                    k8bid, ier)
        call dismoi('F', 'NB_MODES_STA', nomres, 'RESULTAT', nbdef,&
                    k8bid, ier)
        call dismoi('F', 'NB_MODES_DYN', nomres, 'RESULTAT', nbmod,&
                    k8bid, ier)
!
!
        valk(1)=intf
        valk(2)=numref
        valk(3)=raid
        valk(4)=mass
        vali(1)=nbmod
        vali(2)=nbdef
        call u2mesg('I', 'ALGELINE6_2', 4, valk, 2,&
                    vali, 0, 0.d0)
!
!
    endif
!
!   CAS D'UNE BASE DE TYPE CYCLIQUE
!
    if (typeba(1:8) .eq. 'CYCLIQUE') then
        call dismoi('F', 'NB_MODES_TOT', nomres, 'RESULTAT', nbtot,&
                    k8bid, ier)
        call dismoi('F', 'NOM_MODE_CYCL', intf, 'INTERF_DYNA', ibid,&
                    rescyc, iret)
!
        valk(1)=intf
        valk(2)=numref
        call u2mesk('I', 'ALGELINE6_3', 2, valk)
!
    endif
!
! CAS D'UNE BASE DE RITZ
!
    if (typeba(1:4) .eq. 'RITZ') then
!
        call dismoi('F', 'NB_MODES_TOT', nomres, 'RESULTAT', nbtot,&
                    k8bid, ier)
!
        valk(1)=numref
        vali(1)=nbtot
        call u2mesg('I', 'ALGELINE6_4', 1, valk, 1,&
                    vali, 0, 0.d0)
!
    endif
!
    call u2mess('I', 'ALGELINE6_5')
!
    do 10 i = 1, nbtot
!
        call rsadpa(nomres, 'L', nbpabm, bmpara, i,&
                    0, ldpar, k8bid)
!
        typdef=zk16(ldpar(5))
!
        vali(1)=i
        if (typdef .eq. 'PROPRE') then
!
            freq=zr(ldpar(2))
            genek=zr(ldpar(8))
            genem=zr(ldpar(7))
            valr(1)=freq
            valr(2)=genem
            valr(3)=genek
            call u2mesg('I', 'ALGELINE6_6', 0, ' ', 1,&
                        vali, 3, valr)
!
        else
!
            nomnoe=zk16(ldpar(4))(1:8)
            nomcmp=zk16(ldpar(4))(9:16)
            valk(1)=typdef
            valk(2)=nomnoe
            valk(3)=nomcmp
            call u2mesg('I', 'ALGELINE6_7', 3, valk, 1,&
                        vali, 0, 0.d0)
!
        endif
10  end do
!
    call jedema()
end subroutine
