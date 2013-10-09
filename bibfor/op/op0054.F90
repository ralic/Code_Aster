subroutine op0054()
    implicit none
!     ------------------------------------------------------------------
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
!      OPERATEUR :     CALC_THETA
!
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcou2d.h"
#include "asterfort/gcouro.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/gimpte.h"
#include "asterfort/gver2d.h"
#include "asterfort/gverig.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/ulexis.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
!
    real(kind=8) :: module
!
    integer :: nbv, nbr8, nbno, nocc2d, nocc3d, iadrt1
    integer :: iadrno, impr, iadrco, iadrma, iadrt2, iadrt3, iadrt4, iadrt5
    integer :: icode, ific, n1
    real(kind=8) :: dir(3), rinf, rsup
    logical :: ldirec
    character(len=8) :: noma, modele, fond, resu, noeud, format, config
    character(len=16) :: type, oper, fichie, valk(2)
    character(len=24) :: trav1, trav2, trav3, trav4, stok4
    character(len=24) :: obj1, nomno, coorn, obj2, taillr
    character(len=24) :: theta, gdteta
!     ------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    ldirec = .false.
!
!
    call getres(resu, type, oper)
!
    call getvid(' ', 'MODELE', scal=modele, nbret=nbv)
!
    call getfac('THETA_3D', nocc3d)
    call getfac('THETA_2D', nocc2d)
    call getfac('IMPRESSION', impr)
!
    if (impr .ne. 0) then
        call getvtx('IMPRESSION', 'FORMAT ', iocc=1, scal=format, nbret=nbv)
        ific = 0
        fichie = ' '
        call getvis('IMPRESSION', 'UNITE', iocc=1, scal=ific, nbret=n1)
        if (.not. ulexis( ific )) then
            call ulopen(ific, ' ', fichie, 'NEW', 'O')
        endif
    endif
!
! --- CREATION DE LA STRUCTURE DE DONNEES DE TYPE THETA_GEOM QUI EST
! --- ISSUE DE LA COMMANDE :
!     --------------------
    call rscrsd('G', resu, 'THETA_GEOM', 1)
!
! --- CREATION DU NOM DU CHAMP CORRESPONDANT AU NOM SYMBOLIQUE THETA
! --- POUR LE NUMERO D'ORDRE 0 :
!     ------------------------
    call rsexch(' ', resu, 'THETA', 0, theta,&
                icode)
!
! --- SI LE CHAMP THETA EXISTE DEJA, ON SORT EN ERREUR FATALE :
!     -------------------------------------------------------
    if (icode .eq. 0) then
        valk(1)='THETA'
        valk(2)=resu
        call utmess('F', 'RUPTURE1_28', nk=2, valk=valk)
    endif
!
! --- CREATION DU NOM DU CHAMP CORRESPONDANT AU NOM SYMBOLIQUE
! --- GRAD_NOEU_THETA POUR LE NUMERO D'ORDRE 0 :
!     ----------------------------------------
    call rsexch(' ', resu, 'GRAD_NOEU_THETA', 0, gdteta,&
                icode)
!
! --- SI LE CHAMP THETA EXISTE DEJA, ON SORT EN ERREUR FATALE :
!     -------------------------------------------------------
    if (icode .eq. 0) then
        valk(1)='GRAD_NOEU_THETA'
        valk(2)=resu
        call utmess('F', 'RUPTURE1_28', nk=2, valk=valk)
    endif
!
    obj1 = modele//'.MODELE    .LGRF'
    call jeveuo(obj1, 'L', iadrma)
    noma = zk8(iadrma)
    nomno = noma//'.NOMNOE'
    coorn = noma//'.COORDO    .VALE'
    call jeveuo(coorn, 'L', iadrco)
!
!     ==================================================================
!                          T H E T A _ 3 D
!     ==================================================================
!
    if (nocc3d .ne. 0) then
!
        call getvid(' ', 'FOND_FISS', scal=fond, nbret=nbv)
!
        call getvr8(' ', 'DIRECTION', nbval=0, nbret=nbr8)
!
        if (nbr8 .ne. 0) then
            nbr8 = -nbr8
            if (nbr8 .ne. 3) then
                call utmess('F', 'RUPTURE1_30')
            else
                call getvr8(' ', 'DIRECTION', nbval=3, vect=dir, nbret=nbr8)
                ldirec = .true.
            endif
        endif
!
!        --- OBJET CONTENANT LES NOEUDS DU FOND DE FISSURE ---
!
        obj2 = fond//'.FOND.NOEU'
        call jelira(obj2, 'LONMAX', nbno)
        call jeveuo(obj2, 'L', iadrno)
!
        taillr = fond//'.FOND.TAILLE_R'
        call dismoi('CONFIG_INIT', fond, 'FOND_FISS', repk=config)
        call gverig(noma, nocc3d, obj2, taillr, config,&
                    nbno, nomno, coorn, trav1, trav2,&
                    trav3, trav4)
!
!        --- CALCUL SUIVANT LA METHODE CHOISIE ---
!
        call gcouro('G', theta, noma, nomno, coorn,&
                    nbno, trav1, trav2, trav3, dir,&
                    zk8(iadrno), fond, ldirec, stok4)
!
!        --- IMPRESSION DES OBJETS DECRIVANT LE CHAMP THETA ---
!
        if (impr .ne. 0) then
            call jeveuo(trav1, 'L', iadrt1)
            call jeveuo(trav2, 'L', iadrt2)
            call jeveuo(trav3, 'L', iadrt3)
            call jeveuo(trav4, 'L', iadrt4)
            call jeveuo(stok4, 'L', iadrt5)
            call gimpte(theta(1:8), zr(iadrt1), zr(iadrt2), zr( iadrt3), zk8(iadrno),&
                        zr(iadrt5), zr(iadrt4), nbno, format, ific)
        endif
!
    endif
!
!     ==================================================================
!                          T H E T A _ 2 D
!     ==================================================================
!
!
    if (nocc2d .ne. 0) then
!
        call getvr8(' ', 'DIRECTION', nbval=0, nbret=nbr8)
!
        if (nbr8 .ne. 0) then
            nbr8 = -nbr8
            if (nbr8 .ne. 3) then
                call utmess('F', 'RUPTURE1_30')
            else
                call getvr8(' ', 'DIRECTION', nbval=3, vect=dir, nbret=nbr8)
                ldirec = .true.
            endif
        else
            call utmess('F', 'RUPTURE0_81')
        endif
!
        call gver2d(noma, nocc2d, 'THETA_2D', nomno, noeud,&
                    rinf, rsup, module)
!
!        --- CALCUL SUIVANT LA METHODE CHOISIE ---
!
        call gcou2d('G', theta, noma, nomno, noeud,&
                    zr(iadrco), rinf, rsup, module, ldirec,&
                    dir)
!
    endif
!
! --- AFFECTATION DU CHAMNO THETA A LA S.D. RESU DE TYPE THETA_GEOM :
!     -------------------------------------------------------------
    call rsnoch(resu, 'THETA', 0)
!
    call jedema()
end subroutine
