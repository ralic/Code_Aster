subroutine verigd(nomgdz, lcmp, ncmp, iret)
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
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/kndoub.h'
    include 'asterfort/knincl.h'
    include 'asterfort/lxliis.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: ncmp, iret
    character(len=*) :: nomgdz, lcmp(ncmp)
! ---------------------------------------------------------------------
! BUT: VERIFIER LA COHERENCE D'UNE LISTE DE CMPS D'UNE GRANDEUR.
! ---------------------------------------------------------------------
!     ARGUMENTS:
! LCMP   IN   V(K8) : LISTE DES CMPS A VERIFIER
! NCMP   IN   I     : LONGUEUR DE LA LISTE LCMP
! IRET   OUT  I     : CODE RETOUR :
!                     /0 : OK
!                     /1 : NOMGDZ N'EST PAS UNE GRANDEUR.
!                     /2 : UNE CMP (AU MOINS) EST EN DOUBLE DANS LCMP
!                     /3 : UNE CMP (AU MOINS) DE LCMP N'EST PAS UNE
!                          CMP DE NOMGDZ
!
!  SI IRET>0 : ON EMET SYSTEMATIQUEMENT UNE ALARME.
!              C'EST A L'APPELANT D'ARRETER LE CODE SI NECESSAIRE.
!----------------------------------------------------------------------
    character(len=24) :: valk(2)
!     ------------------------------------------------------------------
    integer :: gd, jcmpgd, ncmpmx, i1, k, ibid
    character(len=8) :: kbid, lcmp2(3000), nomgd
! DEB
    call jemarq()
    iret = 0
    nomgd = nomgdz
!
!
!     1. NOMGD EST BIEN LE NOM D'UNE GRANDEUR :
!     -----------------------------------------
    call jenonu(jexnom('&CATA.GD.NOMGD', nomgd), gd)
    if (gd .eq. 0) then
        call u2mesk('A', 'POSTRELE_57', 1, nomgd)
        iret = 1
        goto 30
    endif
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', jcmpgd)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx, kbid)
!
!
!     2. ON RECOPIE LCMP DANS LCMP2 (K8) :
!     -------------------------------------------
    if (ncmp .gt. 3000) call u2mess('F', 'POSTRELE_13')
    do 10,k = 1,ncmp
    lcmp2(k) = lcmp(k)
    10 end do
!
!
!     3. LCMP2 N'A PAS DE DOUBLONS :
!     -----------------------------
    call kndoub(8, lcmp2, ncmp, i1)
    if (i1 .gt. 0) then
        call u2mesk('A', 'POSTRELE_55', 1, lcmp2(i1))
        iret = 2
        goto 30
    endif
!
!
!     3. LCMP2 EST INCLUSE DANS LA LISTE DES CMPS DE LA GRANDEUR :
!     -----------------------------------------------------------
    if (nomgd(1:5) .ne. 'VARI_') then
        call knincl(8, lcmp2, ncmp, zk8(jcmpgd), ncmpmx,&
                    i1)
        if (i1 .gt. 0) then
            valk(1) = lcmp2(i1)
            valk(2) = nomgd
            call u2mesk('A', 'POSTRELE_56', 2, valk)
            iret = 3
            goto 30
        endif
    else
!       -- POUR NOMGD=VARI_* : CMP='V1','V2',..,'V999'
        do 20,k = 1,ncmp
        call lxliis(lcmp2(k) (2:8), ibid, i1)
        if ((lcmp2(k) (1:1).ne.'V') .or. (i1.gt.0)) then
            valk(1) = lcmp2(k)
            valk(2) = nomgd
            call u2mesk('A', 'POSTRELE_56', 2, valk)
            iret = 3
            goto 30
        endif
20      continue
    endif
!
!
30  continue
    call jedema()
!
end subroutine
