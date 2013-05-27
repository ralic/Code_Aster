subroutine lrmmf1(fid, nomamd, nbrfam, carafa, nbnoeu,&
                  famnoe, nmatyp, jfamma, jnumty, tabaux,&
                  nomgro, numgro, nument, infmed, nivinf,&
                  ifm, vecgrm, nbcgrm)
!
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
! person_in_charge: nicolas.sellenet at edf.fr
!-----------------------------------------------------------------------
!     LECTURE DU MAILLAGE - FORMAT MED - LES FAMILLES - 1
!     -    -     -                 -         -          -
!-----------------------------------------------------------------------
!
! ENTREES :
!   FID    : IDENTIFIANT DU FICHIER MED
!   NOMAMD : NOM DU MAILLAGE MED
!   NBRFAM : NOMBRE DE FAMILLES POUR CE MAILLAGE
!   NBNOEU : NOMBRE DE NOEUDS
!   FAMNOE : NUMERO DE FAMILLE POUR CHAQUE NOEUD
!   NMATYP : NOMBRE DE MAILLES DU MAILLAGE PAR TYPE DE MAILLES
!   JFAMMA : POUR UN TYPE DE MAILLE, ADRESSE DANS LE TABLEAU DES
!            FAMILLES D'ENTITES
!   JNUMTY : POUR UN TYPE DE MAILLE, ADRESSE DANS LE TABLEAU DES
!            RENUMEROTATIONS
! SORTIES :
!   NOMGRO : COLLECTION DES NOMS DES GROUPES A CREER
!   NUMGRO : COLLECTION DES NUMEROS DES GROUPES A CREER
!   NUMENT : COLLECTION DES NUMEROS DES ENTITES DANS LES GROUPES
! TABLEAUX DE TRAVAIL
!   CARAFA : CARACTERISTIQUES DE CHAQUE FAMILLE
!     CARAFA(1,I) = NOMBRE DE GROUPES
!     CARAFA(2,I) = NOMBRE D'ATTRIBUTS
!     CARAFA(3,I) = NOMBRE D'ENTITES
!   TABAUX :
! DIVERS
!   INFMED : NIVEAU DES INFORMATIONS SPECIFIQUES A MED A IMPRIMER
!   NIVINF : NIVEAU DES INFORMATIONS GENERALES
!   IFM    : UNITE LOGIQUE DU FICHIER DE MESSAGE
!-----------------------------------------------------------------------
!
    implicit none
!
    include 'jeveux.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/lrmmf2.h'
    include 'asterfort/lrmmf3.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/wkvect.h'
    integer :: ntymax
    parameter (ntymax = 69)
!
! 0.1. ==> ARGUMENTS
!
    integer :: fid
    integer :: nbrfam, carafa(3, nbrfam), nbcgrm
    integer :: nbnoeu, famnoe(nbnoeu)
    integer :: nmatyp(ntymax), jfamma(ntymax), jnumty(ntymax)
    integer :: tabaux(*)
    integer :: infmed
    integer :: ifm, nivinf
!
    character(len=*) :: nomgro, numgro, nument
    character(len=*) :: nomamd, vecgrm
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'LRMMF1' )
!
    integer :: iaux
    integer :: rangfa
    integer :: adnogr, advala
    integer :: nbgrmx, nbatmx, ibid, nbgrlo, jnogrl, jnogrc, jaux
!
    character(len=80) :: mk(3)
    character(len=24) :: vaatfa, nogrfa, nogrlo, nogrco
!
!     ------------------------------------------------------------------
!
    if (nivinf .ge. 2) then
!
        write (ifm,1001) nompro
        1001 format( 60('-'),/,'DEBUT DU PROGRAMME ',a)
!
    endif
!
!====
! 1. NOMBRE DE GROUPES ET D'ATTRIBUTS PAR FAMILLE
!====
!
    call lrmmf2(fid, nomamd, nbrfam, carafa, nbgrmx,&
                nbatmx, infmed, nivinf, ifm)
!
!====
! 2. ON ALLOUE AU MINIMUM A 1 LES LISTES CORRESPONDANT AUX GROUPES
!    ET AUX ATTRIBUTS, POUR QUE TOUT FONCTIONNE BIEN QUAND IL N'Y
!    EN A PAS.
!====
!               12   345678   9012345678901234
    vaatfa = '&&'//nompro//'.VAL_AT_FAM     '
    nogrfa = '&&'//nompro//'.NOM_GR_FAM     '
!
    iaux = max ( nbgrmx, 1 )
    call wkvect(nogrfa, 'V V K80', iaux, adnogr)
!
    iaux = max ( nbatmx, 1 )
    call wkvect(vaatfa, 'V V I', iaux, advala)
!
!====
! 3. DECODAGE DE CHAQUE FAMILLE
!====
!
    nogrlo = '&&LRMMF1.NOM_GR_LONG    '
    call wkvect(nogrlo, 'V V K80', nbrfam, ibid)
    nogrco = '&&LRMMF1.NOM_GR_COURT   '
    call wkvect(nogrco, 'V V K24', nbrfam, ibid)
    nbgrlo = 0
    do 3 , iaux = 1 , nbrfam
!
    rangfa = iaux
!
    call lrmmf3(fid, nomamd, rangfa, carafa, nbnoeu,&
                famnoe, nmatyp, jfamma, jnumty, zi(advala),&
                zk80(adnogr), tabaux, nomgro, numgro, nument,&
                infmed, nivinf, ifm, vecgrm, nbcgrm,&
                nbgrlo)
!
    3 end do
!
    call jeveuo(nogrlo, 'L', jnogrl)
    call jeveuo(nogrco, 'L', jnogrc)
    do 4 , iaux = 1 , nbgrlo
    if (zk80(jnogrl-1+iaux) .ne. zk24(jnogrc-1+iaux)) then
        do 5 , jaux = 1, nbgrlo
        if (jaux .eq. iaux) goto 5
        if ((zk24(jnogrc-1+iaux).eq.zk24(jnogrc-1+jaux)) .and.&
            (zk80(jnogrl-1+iaux).ne.zk80(jnogrl-1+jaux))) then
            mk(1) = zk80(jnogrl-1+iaux)
            mk(2) = zk80(jnogrl-1+jaux)
            mk(3) = zk24(jnogrc-1+iaux)
            call u2mesk('F', 'MED2_1', 3, mk)
        endif
 5      continue
    endif
    4 end do
    call jedetr(nogrlo)
    call jedetr(nogrco)
!
!====
! 4. LA FIN
!====
!
    call jedetr(vaatfa)
    call jedetr(nogrfa)
!
    if (infmed .ge. 3) then
!
        write (ifm,4001)
        do 41 , iaux = 1 , nbrfam
        write (ifm,4002) iaux, carafa(1,iaux), carafa(2,iaux),&
            carafa(3,iaux)
        41     end do
        write (ifm,4003)
!
    endif
    4001 format(&
     &  4x,53('*'),&
     &/,4x,'*   RANG DE  *              NOMBRE DE               *',&
     &/,4x,'* LA FAMILLE *  GROUPES   * ATTRIBUTS  *   ENTITES  *',&
     &/,4x,53('*'))
    4002 format(4x,'*',i9,'   *',i9,'   *',i9,'   *',i9,'   *')
    4003 format(4x,53('*'))
!
    if (nivinf .ge. 2) then
        write (ifm,4000) nompro
        4000 format(/,'FIN DU PROGRAMME ',a,/,60('-'))
    endif
!
end subroutine
