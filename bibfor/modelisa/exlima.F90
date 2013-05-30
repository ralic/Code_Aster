subroutine exlima(motfaz, iocc, base, modelz, ligrel)
    implicit   none
    include 'jeveux.h'
    include 'asterc/getexm.h'
    include 'asterc/getres.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exlim1.h'
    include 'asterfort/gnoms2.h'
    include 'asterfort/gnomsd.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/reliem.h'
    include 'asterfort/u2mess.h'
    character(len=*) :: motfaz, base, modelz, ligrel
    integer :: iocc
!     -----------------------------------------------------------------
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
! BUT  :  SCRUTER LES MOTS CLE TOUT/GROUP_MA/MAILLE POUR CREER
!         UN LIGREL "REDUIT" A PARTIR DU LIGREL DU MODELE MODELZ
!
! IN  : MODELZ : NOM DU MODELE
!
! OUT/JXOUT   : LIGREL  : LIGREL REDUIT
!     ATTENTION :
!          - LE NOM DE LIGREL EST TOUJOURS "OUT"
!          - PARFOIS ON REND LIGREL=LIGREL(MODELE) :
!             - ALORS ON NE TIENT DONC PAS COMPTE DE 'BASE'
!             - IL NE FAUT PAS LE DETRUIRE !
!          - PARFOIS ON EN CREE UN NOUVEAU SUR LA BASE 'BASE'
!             - LE NOM DU LIGREL EST OBTENU PAR GNOMSD
!     -----------------------------------------------------------------
!
    integer :: ib, n1, jma, nbma
    character(len=8) :: modele, noma, k8bid
    character(len=16) :: motfac, motcle(2), typmcl(2), oper, k16b
    character(len=19) :: ligrmo
    character(len=24) :: lismai, noojb
    integer :: iarg
!     -----------------------------------------------------------------
!
    motfac = motfaz
    modele = modelz
    if (modele .eq. ' ') call u2mess('F', 'UTILITAI8_10')
!
    call dismoi('F', 'NOM_LIGREL', modele, 'MODELE', ib,&
                ligrmo, ib)
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ib,&
                noma, ib)
    lismai = '&&EXLIMA.LISTE_MAILLES'
!
!
!     --  SI ON DOIT TOUT PRENDRE , LIGREL = LIGRMO
!     ------------------------------------------------------
    if (motfac .ne. ' ') then
        if (getexm(motfac,'TOUT') .eq. 1) then
            call getvtx(motfac, 'TOUT', iocc, iarg, 0,&
                        k8bid, n1)
            if (n1 .ne. 0) goto 9998
        endif
    else
        call getvtx(' ', 'TOUT', 1, iarg, 0,&
                    k8bid, n1)
        if (n1 .ne. 0) goto 9998
    endif
!
!
!
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
! --- CREATION ET AFFECTATION DU VECTEUR DE K8 DE NOM LISMAI
!     CONTENANT LES NOMS DES MAILLES FORMANT LE LIGREL A CREER
!     --------------------------------------------------------
    call reliem(modele, noma, 'NU_MAILLE', motfac, iocc,&
                2, motcle(1), typmcl(1), lismai, nbma)
!
!     -- SI LES MOTS CLES GROUP_MA ET MAILLE N'ONT PAS ETE UTILISES:
    if (nbma .eq. 0) goto 9998
!
!
!
! --- CREATION DU LIGREL
!     ---------------------------------
    call getres(k16b, k16b, oper)
    if (oper .ne. 'IMPR_RESU') then
        noojb='12345678.LIGR000000.LIEL'
        call gnomsd(' ', noojb, 14, 19)
    else
!     -- DANS LE CAS IMPR_RESU, GNOMSD NE PEUT PAS SERVIR CAR
!        LA COMMANDE NE CREE PAS DE CONCEPT
        call assert(base.eq.'V')
        noojb='&&EXLIMA.LIGR000000.LIEL'
        call gnoms2(noojb, 14, 19)
    endif
    ligrel=noojb(1:19)
    call assert(ligrel(1:8).ne.' ')
    call jeveuo(lismai, 'L', jma)
    call exlim1(zi(jma), nbma, modele, base, ligrel)
    call jedetr(lismai)
    goto 9999
!
!
9998  continue
    ligrel = ligrmo
!
9999  continue
!
!
end subroutine
