subroutine ligecp(noma, nbtout, lonlig, lonema, nbgrel)
    implicit none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/palima.h'
    character(len=8) :: noma
    integer :: nbtout(10), lonlig, lonema, nbgrel
!-----------------------------------------------------------------------
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
! BUT : CALCULER LES DIMENSIONS DU LIGREL DE CHARGE POUR ECHANGE_PAROI
!
! ARGUMENTS D'ENTREE:
!      NOMA   : NOM DU MAILLAGE
! ARGUMENTS DE SORTIE:
!      NBTOUT : (1) =NBRE D'APPEL A NOCART = NBGREL
!               (2) =NBRE GRELS OU DE TYPE_MAIL
!               (3) =NBRE TOTAL DE MAILLES
!               (6) =SOMME SUR TOUTES LES MAILLES DE LEUR NBRE DE NOEUDS
!      LONLIG : DIMENSION DE L'OBJET .LIEL
!      LONEMA : DIMENSION DE L'OBJET .NEMA
!      NBGREL : NOMBRE DE GREL DANS .LIEL
!
! ROUTINES APPELEES:
! JECREO    JEECRA    JEVEUO    GETVID    GETVR8    JEXNOM    *
! JELIRA    JEDETR                                            *
!                                                             *
    integer :: nechpa
    character(len=24) :: liste
! --- DEBUT
!-----------------------------------------------------------------------
    integer :: i, iima, iliste, ima, inbno, iocc, iret
    integer :: itypc, itype, nbma, nbnoma
!-----------------------------------------------------------------------
    call jemarq()
    do 1 i = 1, 10
        nbtout(i) = 0
 1  end do
    lonlig=0
    lonema=0
    nbgrel=0
    liste ='&&LIGECHP'
!
!     LECTURE DE LA IEME OCCURENCE DE :
!     ECHANGE_PAROI : ( GROUP_MA_1 : ...   , MAILLE_1 : ... )
!
    call getfac('ECHANGE_PAROI', nechpa)
    do 2 iocc = 1, nechpa
!
!     CREATION DE L'OBJET &&LIGECHP SUR 'V' QUI CONTIENT LA LISTE DES
!     MAILLES DESCRITES PAR LES MOTS CLES GROUP_MA_1 ET MAILLE_1 ET
!     LE TYPE_MAIL ASSOCIEES A CHACUNE(LES MAILLES SONT REGROUPEES PAR
!     TYPE_MAIL)
!
        call palima(noma, 'ECHANGE_PAROI', 'GROUP_MA_1', 'MAILLE_1', iocc,&
                    liste)
        call jeexin(liste, iret)
        call assert(iret.ne.0)
        call jeveuo(liste, 'L', iliste)
        nbma = zi(iliste)
        nbtout(3) =nbtout(3) +nbma
        itypc = -1
        itype = 0
        do 3 ima = 1, nbma
            iima = 2*(ima-1)+1
            itype = zi(iliste+iima+1)
            if (itype .ne. itypc) then
                nbgrel = nbgrel+1
                itypc = itype
!           NBNOMA = ""NB DE NOEUDS DE LA MAILLE"""""
                call jeveuo(jexnum('&CATA.TM.NBNO', itype), 'L', inbno)
                nbnoma = zi(inbno)
            endif
            lonema = lonema + 2*nbnoma + 1
            nbtout(6) = nbtout(6) + nbnoma
 3      continue
        lonlig = lonlig + nbma + 1
 2  end do
    call jedetr(liste)
    nbtout(1) = nbgrel
    nbtout(2) = nbgrel
    call jedema()
end subroutine
