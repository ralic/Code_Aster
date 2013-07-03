subroutine stock(resu, chs, nocham, ligrel, tychas,&
                 numord, iouf, numode, masgen, amrge,&
                 prchno)
    implicit none
#include "jeveux.h"
!
#include "asterc/r8depi.h"
#include "asterfort/assert.h"
#include "asterfort/cescel.h"
#include "asterfort/cnscno.h"
#include "asterfort/infniv.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsexpa.h"
#include "asterfort/rsnoch.h"
#include "asterfort/u2mesg.h"
    integer :: numord, numode
    real(kind=8) :: iouf, masgen, amrge
    character(len=*) :: resu, chs, nocham, ligrel, tychas
!
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
!----------------------------------------------------------------------
!     TRANSFERT DES RESULTATS DU CHAMP SIMPLE VERS LE CHAMP VRAI
!     PRESENT DANS LA STRUCTURE DE DONNEES RESULTATS
!
! IN  : CHS    : K19 : NOM DU CHAMP SIMPLE
! IN  : TYCHAS : K4  : TYPE DU CHAMP 'NOEU' 'ELNO' 'ELGA'
! IN  : NOCHAM : K16 : NOM DU CHAMP LU 'DEPL', 'SIEF_ELNO'
! IN  : NUMORD : I   : NUMERO D'ORDRE
! IN  : RESU   : K8  : NOM DE LA STRUCTURE DE DONNEES RESULTATS
! IN  : IOUF   : R   : IOUF DE L'INSTANT OU DE LA FREQUENCE
! IN  : PRCHNO : K19 : PROFIL DE STOCKAGE
!
!----------------------------------------------------------------------
!
!
!
    integer :: iret, jiouf, iad, nncp
    integer :: vali(2), ibid, nivinf, ifm
    real(kind=8) :: depi
    character(len=8) :: k8b, acce
    character(len=24) :: valk(2)
    character(len=16) :: param
    character(len=19) :: nomch, prchno
!
!- RECHERCHE DU NOM DU CHAMP RESULTAT
    depi = r8depi()
!
    call rsexch(' ', resu, nocham, numord, nomch,&
                iret)
!
    if (iret .eq. 100) then
    else if (iret.eq.0) then
    else if (iret.eq.110) then
        call rsagsd(resu, 0)
        call rsexch(' ', resu, nocham, numord, nomch,&
                    iret)
    else
        valk (1) = resu
        valk (2) = nomch
        vali (1) = numord
        vali (2) = iret
        call u2mesg('F', 'PREPOST5_73', 2, valk, 2,&
                    vali, 0, 0.d0)
    endif
!
! - TRANSFERT DU CHAMP SIMPLE VERS LE CHAMP VRAI
!
!      CALL UTIMSD('MESSAGE',0,.TRUE.,.TRUE.,PRCHNO,1,' ')
!      CALL UTIMSD('MESSAGE',1,.TRUE.,.TRUE.,
!     &             PRCHNO//".PRNO",1,' ')
    if (tychas .eq. 'NOEU') then
        call cnscno(chs, prchno, 'NON', 'G', nomch,&
                    'F', ibid)
    else
        call cescel(chs, ligrel, ' ', ' ', 'OUI',&
                    nncp, 'G', nomch, 'F', ibid)
    endif
!
!
!-    ON NOTE LE CHAMP
!     ---------------------------------
    call infniv(ifm, nivinf)
    if (nivinf .ge. 1) then
        write (ifm,*) '<LRIDEA> LECTURE DU CHAMP  : ',nocham,numord
    endif
    call rsnoch(resu, nocham, numord)
!
!
!-    S: ON STOCKE LES PARAMETRES :
!     ---------------------------------
!
!     S.1 : INST OU FREQ :
!     --------------------
    acce = 'INST'
    call rsexpa(resu, 0, 'FREQ', iret)
    if (iret .gt. 0) acce = 'FREQ'
!
    call rsexpa(resu, 0, acce, iret)
    call assert(iret.gt.0)
    call rsadpa(resu, 'E', 1, acce, numord,&
                0, jiouf, k8b)
    zr(jiouf) = iouf
!
!     S.2 : NUME_MODE, MASS_GENE et AMOR_GENE :
!     ------------------------------
    param = 'NUME_MODE'
    call rsexpa(resu, 2, param, iret)
    if (iret .gt. 0) then
        call rsadpa(resu, 'E', 1, param, numord,&
                    0, iad, k8b)
        zi(iad) = numode
    endif
!
    param = 'MASS_GENE'
    call rsexpa(resu, 2, param, iret)
    if (iret .gt. 0) then
        call rsadpa(resu, 'E', 1, param, numord,&
                    0, iad, k8b)
        zr(iad) = masgen
    endif
!
    param = 'AMOR_REDUIT'
    call rsexpa(resu, 2, param, iret)
    if (iret .gt. 0) then
        call rsadpa(resu, 'E', 1, param, numord,&
                    0, iad, k8b)
        zr(iad) = amrge
    endif
!
    param = 'AMOR_GENE'
    call rsexpa(resu, 2, param, iret)
    if (iret .gt. 0) then
        call rsadpa(resu, 'E', 1, param, numord,&
                    0, iad, k8b)
        if (amrge .lt. 1.d92) then
            zr(iad) = 2*amrge*masgen*depi*iouf
        else
            zr(iad) = 0.d0
        endif
    endif
!
    param = 'RIGI_GENE'
    call rsexpa(resu, 2, param, iret)
    if (iret .gt. 0) then
        call rsadpa(resu, 'E', 1, param, numord,&
                    0, iad, k8b)
        if (masgen .lt. 1.d92) then
            zr(iad) = masgen*(depi*iouf)**2
        else
            zr(iad) = 0.d0
        endif
    endif
!
    param = 'OMEGA2'
    call rsexpa(resu, 2, param, iret)
    if (iret .gt. 0) then
        call rsadpa(resu, 'E', 1, param, numord,&
                    0, iad, k8b)
        zr(iad) = (depi*iouf)**2
    endif
!
!
!
end subroutine
