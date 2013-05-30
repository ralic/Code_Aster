subroutine op0011()
    implicit  none
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
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/crsolv.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/numddl.h'
    include 'asterfort/numero.h'
    include 'asterfort/promor.h'
    include 'asterfort/uttcpu.h'
    include 'asterfort/wkvect.h'
    integer :: nlimat, imatel
    parameter   (nlimat=100)
    integer :: ifm, nbid, nbmat, niv, nbcha, iacha, jnslv, il
    character(len=2) :: base
    character(len=8) :: k8b, tlimat(nlimat), nuuti, renum, mo
    character(len=14) :: nudev
    character(len=16) :: type, oper, method
    character(len=19) :: ch19, solveu
    character(len=24) :: charge
    integer :: iarg
!----------------------------------------------------------------------
    call infmaj()
    call infniv(ifm, niv)
!
    call getvtx(' ', 'METHODE', 0, iarg, 1,&
                method, nbid)
    call getvtx(' ', 'RENUM', 0, iarg, 1,&
                renum, nbid)
!
    charge = '&&OP0011.CHARGES   .LCHA'
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
    call crsolv(method, renum, solveu, 'G')
!
!
! --- TRAITEMENT DU MOT CLE MATR_RIGI OU MODELE :
!     -----------------------------------------
    call getvid(' ', 'MATR_RIGI', 0, iarg, 0,&
                k8b, nbmat)
!
    if (nbmat .eq. 0) then
        call getvid(' ', 'MODELE', 1, iarg, 1,&
                    mo, nbid)
        call getvid(' ', 'CHARGE', 1, iarg, 0,&
                    k8b, nbcha)
        nbcha = -nbcha
        if (nbcha .ne. 0) then
            call wkvect(charge, 'V V K24', nbcha, iacha)
            call getvid(' ', 'CHARGE', 1, iarg, nbcha,&
                        zk24(iacha), nbid)
        endif
        call numero(' ', mo, charge(1:19), solveu, base,&
                    nudev)
        call jedetr(charge)
        goto 20
    endif
!
!
    nbmat = -nbmat
    call getvid(' ', 'MATR_RIGI', 0, iarg, nbmat,&
                tlimat, nbmat)
    call wkvect('&&OP001_LIST_MATEL', 'V V K24', nbmat, imatel)
    do 10 il = 1, nbmat
        zk24(imatel+il-1)=tlimat(il)
10  end do
!
!
    call uttcpu('CPU.RESO.1', 'DEBUT', ' ')
    call uttcpu('CPU.RESO.2', 'DEBUT', ' ')
!
! --- CALCUL DE LA NUMEROTATION PROPREMENT DITE :
!     -----------------------------------------
    call numddl(nudev, 'GG', nbmat, zk24(imatel), renum)
!
! --- CREATION ET CALCUL DU STOCKAGE MORSE DE LA MATRICE :
!     -----------------------------------------------------------
    call promor(nudev, 'G')
!
    call uttcpu('CPU.RESO.1', 'FIN', ' ')
    call uttcpu('CPU.RESO.2', 'FIN', ' ')
!
!
! --- CREATION DE L'OBJET .NSLV :
!     -------------------------------------
    call wkvect(nudev//'.NSLV', 'G V K24', 1, jnslv)
    zk24(jnslv-1+1)=solveu
!
!
20  continue
!
!
!
! --- MENAGE :
!     ------
    ch19 = nudev
    call jedetr(ch19(1:14)//'.NEWN')
    call jedetr(ch19(1:14)//'.OLDN')
    call jedetr(ch19//'.ADNE')
    call jedetr(ch19//'.ADLI')
end subroutine
