subroutine irmopr(chamns, nbnoec, linoec, nonvpr)
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/wkvect.h'
    integer :: linoec(*), nbnoec
    character(len=19) :: chamns, nonvpr
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
! ----------------------------------------------------------------------
!  IMPR_RESU - MODIFICATION DU PROFIL
!  -    -      --              --
! ----------------------------------------------------------------------
!
!  CETTE ROUTINE SERT A MODIFIER LE PROFIL POUR LES CHAMPS AUX NOEUDS
!   DANS LE CAS OU CERTAINS NOEUDS DEVRAIENT ETRE EXCLUS
!   EXEMPLE : POUR LES QUAD9, LE CHAMP DEPL N'A PAS DE VALEUR SUR LE
!             DERNIER NOEUD MILIEU
!
! IN  :
!   CHAMNS  K19  NOM DU CHAM_NO_S A ANALYSER
!   NBNOEC  I    NOMBRE DE NOEUDS DEJA PRESENTS DANS LE PROFIL
!   LINOEC  I*   LISTE DES NOEUDS DANS LE PROFIL
!
! OUT :
!   NONVPR  K19  NOM DE L'OBJET JEVEUX CONTENANT LE PROFIL MODIFIE
!                 EN PREMIERE POSITION ON TROUVE LA TAILLE DU PROFIL
!                 SI ON A RETIRE AUCUN NOEUD PAR RAPPORT AU MAILLAGE
!                 INITIAL ALORS ON N'A PAS BESOIN DE PROFIL
!                 => TAILLE DU PROFIL = 0
! person_in_charge: nicolas.sellenet at edf.fr
!     ------------------------------------------------------------------
!
    integer :: nbnoto, nbcmpt, jcnsl, jcnsd, iaux, ino, jnvpro, nbnonp
!
    call jemarq()
!
    call jeveuo(chamns//'.CNSL', 'L', jcnsl)
    call jeveuo(chamns//'.CNSD', 'L', jcnsd)
    nbcmpt=zi(jcnsd+1)
    if (nbnoec .eq. 0) then
        nbnoto=zi(jcnsd)
    else
        nbnoto=nbnoec
    endif
!
    call wkvect(nonvpr, 'V V I', nbnoto+1, jnvpro)
!
    nbnonp=0
    if (nbnoec .eq. 0) then
        do 10, ino=1,nbnoto
        if (zl(jcnsl+(ino-1)*nbcmpt)) then
            zi(jnvpro+nbnonp+1)=ino
            nbnonp=nbnonp+1
        endif
10      continue
        call assert(nbnonp.le.nbnoto)
        if (nbnonp .eq. nbnoto) then
            zi(jnvpro)=0
        else
            zi(jnvpro)=nbnonp
        endif
    else
        do 20, iaux=1,nbnoto
        ino=linoec(iaux)
        if (zl(jcnsl+(ino-1)*nbcmpt)) then
            zi(jnvpro+nbnonp+1)=ino
            nbnonp=nbnonp+1
        endif
20      continue
        call assert(nbnonp.le.nbnoec)
        zi(jnvpro)=nbnonp
    endif
!
    call jedema()
!
end subroutine
