subroutine defdda(nbec, nbcmp, numgd, ioc, motcle,&
                  iopt, icod)
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
!    P. RICHARD     DATE 18/02/91
!-----------------------------------------------------------------------
!  BUT:  DETERMINER LA LISTE DES TYPE DDL DEFINIS PAR L'UTILISATEUR
    implicit none
!       EN ARGUMENT D'UN MOT-CLE
!     INDEPENDAMENT DES DDL ACTIFS DANS LE MODELE
!             IL SORT UN ENTIER CODE
!  TRAITEMENT DU CAS DE L'ABSENCE DE MOT-CLE PAR IOPT
!-----------------------------------------------------------------------
!
! NBEC     /I/: NOMBRE D'ENTIER CODES GRANDEUR SOUS-JACENTE
! NBCMP    /I/: NOMBRE DE COMPOSANTE MAX DE LA GRANDEUR SOUS-JACENTE
! NUMGD    /I/: NUMERO DE LA GRANDEUR SOUS-JACENTE
! IOC      /I/: NUMERO OCCURENCE MOTFAC INTERFACE DEFINISSANT LES DDL
! MOTCLE   /I/: MOT CLE
! IOPT     /I/: CODE POUR ABSENCE MOT-CLE (1 TOUT DDL) (0 AUCUN DDL)
! ICOD     /O/: ENTIER CODE
!
!
!
!
!
    include 'jeveux.h'
!
    include 'asterc/getvtx.h'
    include 'asterfort/iscode.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: nomcou
    character(len=*) :: motcle
    character(len=24) :: temddl, temidc
    character(len=24) :: valk
    character(len=80) :: kar80
    integer :: icod(nbec)
    logical :: ok, okg
    integer :: iarg
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, iec, ioc, iopt, j, llncmp
    integer :: ltddl, ltidec, nbcmp, nbec, nbval, numgd
!-----------------------------------------------------------------------
    data okg/.false./
!-----------------------------------------------------------------------
!
    call jemarq()
!
    if (motcle(1:9) .eq. 'DDL_ACTIF') then
        nbval = 0
    else
        call getvtx('INTERFACE', motcle, ioc, iarg, 0,&
                    kar80, nbval)
        nbval = -nbval
    endif
!
!----------ALLOCATION DU VECTEUR DES ENTIERS DE DECODAGE----------------
!
    temidc = '&&DEFDDA.IDEC'
    call wkvect(temidc, 'V V I', nbcmp, ltidec)
!
!--------------TRAITEMENT DES EXCEPTIONS: PAS DE MOT CLE----------------
!
    if (nbval .eq. 0 .and. iopt .eq. 1) then
        do 30 i = 1, nbcmp
            zi(ltidec+i-1) = 1
30      continue
        call iscode(zi(ltidec), icod, nbcmp)
        goto 9999
    endif
!
    if (nbval .eq. 0 .and. iopt .eq. 0) then
        do 40 iec = 1, nbec
            icod(iec) = 0
40      continue
        goto 9999
    endif
!
!---------RECUPERATION DU VECTEUR DES NOMS DE COMPOSANTES---------------
!
    call jeveuo(jexnum('&CATA.GD.NOMCMP', numgd), 'L', llncmp)
!
    temddl = '&&DEFDDA.DDL.DON'
    call wkvect(temddl, 'V V K80', nbval, ltddl)
!
    if (motcle(1:9) .eq. 'DDL_ACTIF') then
        ibid = 0
    else
        call getvtx('INTERFACE', motcle, ioc, iarg, nbval,&
                    zk80(ltddl), ibid)
    endif
!
    do 10 i = 1, nbval
        nomcou = zk80(ltddl+i-1)
        ok = .true.
        do 20 j = 1, nbcmp
            if (nomcou .eq. zk8(llncmp+j-1)) then
                zi(ltidec+j-1) = 1
                ok = .false.
            endif
20      continue
!
        if (ok) then
            okg = .true.
            valk = nomcou
            call u2mesg('E+', 'ALGORITH15_8', 1, valk, 0,&
                        0, 0, 0.d0)
            call u2mesg('E', 'VIDE_1', 0, ' ', 0,&
                        0, 0, 0.d0)
        endif
!
10  end do
!
    if (okg) then
        call u2mesg('F', 'ALGORITH15_10', 0, ' ', 0,&
                    0, 0, 0.d0)
    endif
!
    call iscode(zi(ltidec), icod, nbcmp)
!
    call jedetr(temddl)
!
9999  continue
    call jedetr(temidc)
!
    call jedema()
end subroutine
