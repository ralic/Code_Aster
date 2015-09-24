subroutine op0011()
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!======================================================================
!
!                       OPERATEUR NUME_DDL
!======================================================================
!
!
!======================================================================
!----------------------------------------------------------------------
!     VARIABLES LOCALES
!----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/crsolv.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedetr.h"
#include "asterfort/numddl.h"
#include "asterfort/numero.h"
#include "asterfort/promor.h"
#include "asterfort/uttcpu.h"
#include "asterfort/wkvect.h"
    integer :: nlimat, imatel
    parameter   (nlimat=100)
    integer :: ifm, nbid, nbmat, niv, nbcha, iacha, jnslv, il
    real(kind=8) :: blreps, blrfront
    character(len=2) :: base
    character(len=8) ::  tlimat(nlimat), nuuti, renum, mo
    character(len=14) :: nudev
    character(len=16) :: type, oper, method
    character(len=19) :: ch19, solveu
    character(len=24) :: list_load
!----------------------------------------------------------------------
    call infmaj()
    call infniv(ifm, niv)
!
    call getvtx(' ', 'METHODE', iocc=1, scal=method, nbret=nbid)
    ASSERT(nbid.eq.1)
    call getvtx(' ', 'RENUM', iocc=1, scal=renum, nbret=nbid)
    ASSERT(nbid.eq.1)
    blrfront=0.d0
    blreps=0.d0
    if (method(1:5).eq.'MUMPS') then
        call getvr8(' ', 'LOW_RANK_TAILLE', iocc=1, scal=blrfront, nbret=nbid)
        ASSERT(nbid.eq.1)
        call getvr8(' ', 'LOW_RANK_SEUIL', iocc=1, scal=blreps, nbret=nbid)
        ASSERT(nbid.eq.1)
    endif
!
    list_load = '&&OP0011.CHARGES   .LCHA'
    base ='GG'
!
! --- RECUPERATION DU CONCEPT RESULTAT ET DE SON NOM UTILISATEUR :
!     ----------------------------------------------------------
    call getres(nuuti, type, oper)
    nudev = nuuti
!
!
!     -- CREATION D'UNE SD SOLVEUR :
!     --------------------------------
    solveu=nuuti//'.SOLVEUR'
    call crsolv(method, renum, blrfront, blreps, solveu, 'G')

!
!
! - TRAITEMENT DU MOT CLE MATR_RIGI OU MODELE :
!
    call getvid(' ', 'MATR_RIGI', nbval=0, nbret=nbmat)
!
    if (nbmat .eq. 0) then
        call getvid(' ', 'MODELE', scal=mo, nbret=nbid)
        call getvid(' ', 'CHARGE', nbval=0, nbret=nbcha)
        nbcha = -nbcha
        if (nbcha .ne. 0) then
            call wkvect(list_load, 'V V K24', nbcha, iacha)
            call getvid(' ', 'CHARGE', nbval=nbcha, vect=zk24(iacha), nbret=nbid)
        endif
        call numero(nudev, solveu, base,&
                    modelz = mo, list_loadz = list_load)
        call jedetr(list_load)
    else
        nbmat = -nbmat
        call getvid(' ', 'MATR_RIGI', nbval=nbmat, vect=tlimat)
        call wkvect('&&OP001_LIST_MATEL', 'V V K24', nbmat, imatel)
        do il = 1, nbmat
            zk24(imatel+il-1)=tlimat(il)
        end do
!
        call uttcpu('CPU.RESO.1', 'DEBUT', ' ')
        call uttcpu('CPU.RESO.2', 'DEBUT', ' ')
!
! ----- CALCUL DE LA NUMEROTATION PROPREMENT DITE :
! 
        call numddl(nudev, 'GG', nbmat, zk24(imatel), renum)
!
! ----- CREATION ET CALCUL DU STOCKAGE MORSE DE LA MATRICE :
!
        call promor(nudev, 'G')
!
        call uttcpu('CPU.RESO.1', 'FIN', ' ')
        call uttcpu('CPU.RESO.2', 'FIN', ' ')
!
! ----- CREATION DE L'OBJET .NSLV :
!
        call wkvect(nudev//'.NSLV', 'G V K24', 1, jnslv)
        zk24(jnslv-1+1)=solveu
!
    endif
!
! - Clean
!
    ch19 = nudev
    call jedetr(ch19(1:14)//'.NEWN')
    call jedetr(ch19(1:14)//'.OLDN')
    call jedetr(ch19//'.ADNE')
    call jedetr(ch19//'.ADLI')
end subroutine
