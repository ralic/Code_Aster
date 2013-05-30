function exicp(modele, mesmai, nbma)
    implicit none
!
    include 'jeveux.h'
!
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/teattr.h'
    character(len=8) :: modele
    character(len=24) :: mesmai
    integer :: nbma
    logical :: exicp
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     RENVOIE .TRUE. SI PARMI LES ELEMENTS FINIS ASSOCIES AUX MAILLES
!     DONNEES, IL EN EXISTE UN QUI SOIT EN C_PLAN
!     SI AUCUNE MAILLE N'EST DONNEE EN ENTREE (NBMA=0), ALORS
!     ON REGARDE TOUTES LES MAILLES DU MAILLAGE
!
! IN  MODELE : NOM DU MODELE
! IN  MESMAI : LISTE DES MAILLES SUR LESQUELLES ON FAIT LE TEST
! IN  NBMA   : NOMBRE DE MAILLES DANS MESMAI
!              SI NBMA = 0  ON FAIT LE TEST SUR TOUTES LES MAILLES DU
!              MAILLAGE ASSOCIÉE AU MODELE
! OUT EXICP : .TRUE. SI C_PLAN TROUVE
!
! ----------------------------------------------------------------------
!
    integer :: nbmat, jma, iret, ima, jtypel, numa, itypel, ibid
    character(len=8) :: noma, k8bid, dmo, dma
    character(len=16) :: notype, typmod
    character(len=24) :: mail
!
!
    call jemarq()
!
    exicp = .false.
!
    if (nbma .ne. 0) then
!       DES MAILLES ONT ETE DONNEES
        call jeveuo(mesmai, 'L', jma)
        nbmat = nbma
    else
!       ON PREND TOUTES LES MAILLES DU MAILLAGE
        call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                    noma, iret)
        call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbmat,&
                    k8bid, iret)
    endif
!
    mail = modele//'.MAILLE'
    call jeveuo(mail, 'L', jtypel)
!
!     BOUCLE SUR LES MAILLES
    do 10 ima = 1, nbmat
!
!       RECUP DU NUMERO DE LA MAILLE COURANTE
        if (nbma .ne. 0) then
            numa = zi(jma-1+ima)
        else
            numa = ima
        endif
!
!       RECUP DU NUMERO DU TYPE D'ELEMENT ASSOCIE
        itypel=zi(jtypel-1+numa)
!
!       SI LA MAILLE N'EST PAS AFFECTEE D'UN ELEMENT FINI ON ZAPPE
        if (itypel .eq. 0) goto 10
!
!       RECUP DU NOM DU TYPE D'ELEMENT ASSOCIE
        call jenuno(jexnum('&CATA.TE.NOMTE', itypel), notype)
!
        call teattr(notype, 'S', 'DIM_TOPO_MODELI', dmo, iret)
        call teattr(notype, 'S', 'DIM_TOPO_MAILLE', dma, iret)
!       SI ELEMENT DE BORD ON ZAPPE
        if (dmo .ne. dma) goto 10
!
        call teattr(notype, 'C', 'TYPMOD', typmod, iret)
!
!       SI L'ATTRIBUT TYPMOD N'EST PAS TROUVE ON ZAPPE
        if (iret .ne. 0) goto 10
!
        if (typmod(1:6) .eq. 'C_PLAN') then
            exicp=.true.
            goto 999
        endif
!
10  end do
!
999  continue
    call jedema()
end function
