subroutine refe99(nomres)
    implicit none
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nomres
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT:
!       RECUPERER LES NOMS UTILISATEUR DES CONCEPTS ASSOCIES AUX
!       MATRICES ASSEMBLEES CONSIDEREES - EFFECTUER QUELQUES CONTROLES
!       CREER LE .REFD
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NOMRES   : NOM DE LA SD_RESULTAT
!
! ......................................................................
!
!
!
!
    character(len=6) :: nompro
    parameter (nompro='REFE99')
!
    integer :: i, ioc1, ioc3, ioc4, ioc5, ioci, iret, ibid
    integer :: ldref, llres, ltmome, nbg, nbmome
!
    character(len=8) :: k8bid, resul
    character(len=8) :: meca
    character(len=19) :: numddl, numbis
    character(len=24) :: raid, mass, intf, amor
    character(len=24) :: valk(4)
!
    logical :: noseul
    integer :: iarg
!
!-----------------------------------------------------------------------
!
    call jemarq()
    numddl = ' '
    raid = ' '
    mass = ' '
    amor = ' '
!
! --- DETERMINATION DU TYPE DE BASE
!
    call getfac('CLASSIQUE', ioc1)
    call getfac('RITZ', ioc3)
    call getfac('DIAG_MASS', ioc4)
    call getfac('ORTHO_BASE', ioc5)
!
! --- CAS CLASSIQUE
!
    if (ioc1 .gt. 0) then
        numbis =' '
        call getvid('CLASSIQUE', 'INTERF_DYNA', 1, iarg, 1,&
                    intf, ibid)
        call dismoi('F', 'NOM_NUME_DDL', intf, 'INTERF_DYNA', ibid,&
                    numddl, iret)
!        NUMDDL(15:19)='.NUME'
        call getvid('CLASSIQUE', 'MODE_MECA', 1, iarg, 0,&
                    k8bid, nbmome)
        nbmome = -nbmome
        call wkvect('&&'//nompro//'.MODE_MECA', 'V V K8', nbmome, ltmome)
        call getvid('CLASSIQUE', 'MODE_MECA', 1, iarg, nbmome,&
                    zk8(ltmome), ibid)
        do 10 i = 1, nbmome
            call jeveuo(zk8(ltmome-1+i)//'           .REFD', 'L', llres)
            raid=zk24(llres)
            mass=zk24(llres+1)
            amor=zk24(llres+2)
            call dismoi('F', 'NOM_NUME_DDL', raid, 'MATR_ASSE', ibid,&
                        numbis, iret)
!          NUMBIS(15:19)='.NUME'
            if (numbis(1:14) .ne. numddl(1:14)) then
                resul = zk8(ltmome-1+i)
                valk (1) = resul
                valk (2) = numbis(1:8)
                valk (3) = intf
                valk (4) = numddl(1:8)
                call u2mesk('F', 'ALGORITH14_24', 4, valk)
            endif
10      continue
        call jedetr('&&'//nompro//'.MODE_MECA')
!
    endif
!
! --- CAS RITZ
!
    if (ioc3 .gt. 0) then
        noseul=.false.
        call getvid('RITZ', 'MODE_MECA', 1, iarg, 999,&
                    k8bid, nbg)
        call getvid('RITZ', 'MODE_INTF', 2, iarg, 0,&
                    k8bid, ibid)
        if ((ibid.gt.0) .or. (nbg.gt.1)) then
            noseul=.true.
        endif
        call getvid('    ', 'NUME_REF', 1, iarg, 1,&
                    numddl, ibid)
        if ((ibid.eq.0) .and. noseul) then
!         si on a plus d'un mode_meca en entree, preciser NUME_REF
            call u2mess('E', 'ALGORITH17_9')
        endif
!        NUMDDL(15:19)='.NUME'
        call getvid('  ', 'INTERF_DYNA', 1, iarg, 0,&
                    intf, ioci)
        if (ioci .lt. 0) then
            call getvid('  ', 'INTERF_DYNA', 1, iarg, 1,&
                        intf, ioci)
        else
            intf=' '
        endif
    endif
!
! --- DIAGONALISATION DE LA MATRICE DE MASSE
!
    if (ioc4 .gt. 0) then
        intf = ' '
! - RECUPERATION DE LA MASSE
        call getvid('DIAG_MASS', 'MODE_MECA', 1, iarg, 1,&
                    meca, ibid)
        call jeveuo(meca//'           .REFD', 'L', llres)
        raid = zk24(llres)
        mass = zk24(llres+1)
        amor = zk24(llres+2)
!
        call dismoi('F', 'NOM_NUME_DDL', mass, 'MATR_ASSE', ibid,&
                    numddl, iret)
    endif
!
! --- CAS ORTHO_BASE
!
    if (ioc5 .gt. 0) then
        intf = ' '
        call getvid('ORTHO_BASE', 'BASE', 1, iarg, 1,&
                    meca, ibid)
        call jeveuo(meca//'           .REFD', 'L', llres)
        raid = zk24(llres)
        mass = zk24(llres+1)
        amor = zk24(llres+2)
!        CALL DISMOI('F','NOM_NUME_DDL',MASS,'MATR_ASSE',IBID,
!     &                 NUMDDL,IRET)
        numddl=zk24(llres+3)
    endif
!
! --- CREATION DU .REFD
!
    call jeexin(nomres//'           .REFD', iret)
    if (iret .eq. 0) then
        call wkvect(nomres//'           .REFD', 'G V K24', 7, ldref)
        zk24(ldref) = raid
        zk24(ldref+1) = mass
        zk24(ldref+2) = amor
        zk24(ldref+3) = numddl(1:14)
        zk24(ldref+4) = intf
        zk24(ldref+5) = '  '
        if (ioc1 .gt. 0) then
            zk24(ldref+6) = 'CLASSIQUE'
        else if (ioc3.gt.0) then
            zk24(ldref+6) = 'RITZ'
        else if (ioc4.gt.0) then
            zk24(ldref+6) = 'DIAG_MASS'
        else if (ioc5.gt.0) then
            zk24(ldref+6) = 'ORTHO_BASE'
        endif
    endif
!
    call jedema()
!
end subroutine
