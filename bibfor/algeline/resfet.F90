subroutine resfet(matas, chcine, chsecm, chsol, niter,&
                  criter, solv19)
!-----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  RESOLUTION D'UN SYSTEME LINEAIRE VIA FETI
!
!     RESOLUTION DU SYSTEME       "NOMMAT" * X = "CHAMNO"
!                                       AVEC X = U0 SUR G
!     ------------------------------------------------------------------
!     IN  MATAS  : CH19 : NOM DE LA MATR_ASSE GLOBALE
!     IN  CHCINE : CH19 : CHAM_NO DE CHARGEMENT CINEMATIQUE
!                         ASSOCIE A LA CONTRAINTE X = U0 SUR G
!     IN CHSECM  : CH19 : CHAM_NO GLOBAL SECOND MEMBRE
!     OUT CHSOL  : CH19 : CHAM_NO GLOBAL SOLUTION
!     IN  NITER  :  IN  : NOMBRE D'ITERATIONS MAXIMALES ADMISSIBLES DU
!                         GCPPC DE FETI
!     IN  CRITER :  K19 : STRUCTURE DE DONNEE STOCKANT INFOS DE CV
!     IN  SOLV19 :  K19 : SOLVEUR
!     ------------------------------------------------------------------
!     VERIFICATIONS :
!     1) SI VCINE = ' ' : ERREUR SI LE NOMBRE DE DDLS IMPOSES ELIMINES
!                         ASSOCIES A LA MATRICE EST /=0
!     2) SI VCINE/= ' ' : ERREUR SI LE NOMBRE DE DDLS IMPOSES ELIMINES
!                         ASSOCIES A LA MATRICE EST =0
!----------------------------------------------------------------------
! CORPS DU PROGRAMME
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
    include 'jeveux.h'
!
    include 'asterfort/alfeti.h'
    include 'asterfort/assde2.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/idensd.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mtdscr.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/uttcpr.h'
    include 'asterfort/uttcpu.h'
    real(kind=8) :: epsi, testco
    character(len=19) :: matas, chcine, chsecm, chsol
    character(len=19) :: criter
!
!
! DECLARATION VARIABLES LOCALES
    integer :: niter, nbreor, nbreoi, reacre, jslvi, jslvk, jslvr
    integer :: idime, nbsd, idd, ifetm, ilimpi, ifetpt, ifetc, iinf
    integer :: ibid, ifm, niv
    real(kind=8) :: rbid, temps(6)
    character(len=19) :: solv19, sdfeti, arg1, arg2, pchn1, pchn2
    character(len=24) :: opt, infofe, valk(2)
    character(len=24) :: tyreor, preco, scalin, stogi, acma, acsm
    logical :: lfetic, iddok
!
!
! CORPS DU PROGRAMME
    call jemarq()
    call infniv(ifm, niv)
!
    call jeveuo(solv19//'.SLVK', 'L', jslvk)
    call jeveuo(solv19//'.SLVR', 'L', jslvr)
    call jeveuo(solv19//'.SLVI', 'L', jslvi)
!
    sdfeti=zk24(jslvk+5)
    epsi=zr(jslvr+1)
    testco=zr(jslvr+3)
    niter=zi(jslvi+1)
    nbreor=zi(jslvi+4)
    nbreoi=zi(jslvi+5)
    preco=zk24(jslvk+1)
    tyreor=zk24(jslvk+6)
    scalin=zk24(jslvk+7)
    stogi=zk24(jslvk+8)
    acma=zk24(jslvk+9)
    acsm=zk24(jslvk+10)
    reacre=zi(jslvi+6)
!
!
    lfetic=.false.
    call jeveuo(sdfeti//'.FDIM', 'L', idime)
!     NOMBRE DE SOUS-DOMAINES
    nbsd=zi(idime)
    call jeveuo(matas//'.FETM', 'L', ifetm)
    call jeveuo(chsecm//'.FETC', 'L', ifetc)
!     ADRESSE JEVEUX OBJET FETI & MPI
    call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
    call jeveuo('&FETI.FINF', 'L', iinf)
    infofe=zk24(iinf)
    if (infofe(11:11) .eq. 'T') lfetic=.true.
!
!
    do 10 idd = 0, nbsd
        if (zi(ilimpi+idd) .eq. 1) then
            iddok=.true.
        else
            iddok=.false.
        endif
        if (iddok) then
!
            if (idd .eq. 0) then
                arg1=matas
                arg2=chsecm
            else
                arg1=zk24(ifetm+idd-1)
                arg2=zk24(ifetc+idd-1)
            endif
            call dismoi('F', 'PROF_CHNO', arg1, 'MATR_ASSE', ibid,&
                        pchn1, ibid)
            call dismoi('F', 'PROF_CHNO', arg2, 'CHAM_NO', ibid,&
                        pchn2, ibid)
            if (.not.idensd('PROF_CHNO',pchn1,pchn2)) then
                valk(1)=arg1
                valk(2)=arg2
                call u2mesg('F', 'FACTOR_61', 2, valk, 1,&
                            idd, 0, rbid)
            endif
        endif
10  end do
!
!
!
    if (lfetic) then
        call uttcpu('CPU.RESFET', 'INIT ', ' ')
        call uttcpu('CPU.RESFET', 'DEBUT', ' ')
    endif
!
!
!
!
!
! ADRESSE JEVEUX OBJET FETI & MPI
    call jeveuo('&FETI.LISTE.SD.MPI', 'L', ilimpi)
!
! PAR SOUCIS D'HOMOGENEITE, MAJ DES OBJETS JEVEUX TEMPORAIRE .&INT
    do 20 idd = 1, nbsd
        if (zi(ilimpi+idd) .eq. 1) call mtdscr(zk24(ifetm+idd-1)(1:19))
20  end do
!
! SOLVEUR FETI SANS AFFE_CHAR_CINE
    call jeveuo('&FETI.PAS.TEMPS', 'E', ifetpt)
! ON INCREMENTE L'INDICE DE L'INCREMENT DE TEMPS POUR MULTIPLE SECONDS
! MEMBRES
    zi(ifetpt+1)=zi(ifetpt+1)+1
    opt='RESOLUTION'
    call alfeti(opt, sdfeti, matas, chsecm, chsol,&
                niter, epsi, criter, testco, nbreor,&
                tyreor, preco, scalin, stogi, nbreoi,&
                acma, acsm, reacre)
!
!     -- DESTRUCTION DES CHAM_NO FILS SOLUTION ET DU .FETC
    call assde2(chsol)
!
    if (lfetic) then
        call uttcpu('CPU.RESFET', 'FIN', ' ')
        call uttcpr('CPU.RESFET', 6, temps)
        write (ifm,'(A44,D11.4,D11.4)')&
     &    'TEMPS CPU/SYS SOLVEUR FETI                : ',temps(5),&
     &    temps(6)
    endif
!
    call jedema()
end subroutine
