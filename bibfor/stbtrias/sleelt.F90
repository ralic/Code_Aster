subroutine sleelt(iunv, maxnod, nbtyma, indic, permut,&
                  nbmail, mint, mant, datset, inum)
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
! aslint: disable=
    implicit none
!     ==============================================================
!A PRESUPER
!
!     ============================================================
!     !                                                          !
!     !  FONCTION:LECTURE DES MAILLES SUR LE FICHIER UNIVERSEL   !
!     !           ISSU DE SUPERTAB I-DEAS 4.0, 6.0 OU 7.0   ET   !
!     !           STOCKAGE DANS DES OBJETS JEVEUX                !
!     !                                                          !
!     ============================================================
!     !                                                          !
!     !  ROUTINES APPELES : IUNIFI (FONCTION)                    !
!     !                          : LECELT                        !
!     !                                                          !
!     !  ROUTINE APPELANTE : PRESUP                              !
!     !                                                          !
!     ============================================================
!     !                                                          !
!     !                   ***************                        !
!     !                   *  ARGUMENTS  *                        !
!     !                   ***************                        !
!     !                                                          !
!     !  ******************************************************  !
!     !  *   NOM    *  TYPE * MODE *ALTERE *      ROLE        *  !
!     !  ******************************************************  !
!     !  *          *       *      *       *                  *  !
!     !  * MAXNOD   *INTEGER*ENTREE* NON   *NBRE MAXI DE NOEUD*  !
!     !  *          *       *      *       * POUR UNE MAILLE  *  !
!     !  * NBTYMA   *INTEGER*ENTREE* NON   *NBRE DE TYPE DE   *  !
!     !  *          *       *      *       *MAILLES SUPERTAB  *  !
!     !  * INDIC    *INTEGER*ENTREE* NON   *INDIQUE SI UNE    *  !
!     !  *          *       *      *       * PERMUTATION EST  *  !
!     !  *          *       *      *       *  NECESSAIRE      *  !
!     !  * PERMUT   *INTEGER*ENTREE* NON   *TABLEAU DES PERMU-*  !
!     !  *          *       *      *       *TATION DES NOEUDS *  !
!     !  *          *       *      *       *POUR UNE MAILLE   *  !
!     !  * NBMAIL   *INTEGER*SORTIE* NON   * TABLEAU CONTENANT*  !
!     !  *          *       *      *       *LE NOMBRE DE MAIL-*  !
!     !  *          *       *      *       *LES DE CHAQUE TYPE*  !
!     !  * MANT     *INTEGER*SORTIE* NON   * TABLEAU CONTENANT*  !
!     !  *          *       *      *       *LE N DE MAILLE MAX*  !
!     !  *          *       *      *       *POUR CHAQUE TYPE  *  !
!     !  *          *       *      *       * DE MAILLE        *  !
!     !  * MINT     *INTEGER*SORTIE* NON   * TABLEAU CONTENANT*  !
!     !  *          *       *      *       *LE N DE MAILLE MIN*  !
!     !  *          *       *      *       *POUR CHAQUE TYPE  *  !
!     !  *          *       *      *       * DE MAILLE        *  !
!     !  *          *       *      *       * DE MAILLE        *  !
!     !  * DATSET   *INTEGER*ENTREE* NON   * NUMERO DU DATASET*  !
!     !  *          *       *      *       *TRAITE            *  !
!     !  *          *       *      *       *                  *  !
!     !  ******************************************************  !
!     !                                                          !
!     ============================================================
!
! ---------------------------------------------------------------
!  --> DECLARATION DES ARGUMENTS
#include "jeveux.h"
#include "asterfort/cov4v5.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/juveca.h"
#include "asterfort/lecelt.h"
#include "asterfort/lect82.h"
#include "asterfort/wkvect.h"
    integer :: maxnod, nbtyma, datset
    integer :: mint(nbtyma), mant(nbtyma)
    integer :: nbmail(nbtyma), indic(nbtyma), permut(maxnod, nbtyma)
!  --> DECLARATION DES VARIABLES LOCALES
    character(len=80) :: cbuf
    integer :: ind, jnum, codgra, codmec, iprop, imat, icol, nbnode, inum
    integer :: node(32), nod82(10000), ico, ibid2, icp, ino
    integer :: coddes, iphyb, imatb, nsizec, nsizei, nn, it
!
!  --------- FIN DECLARATION ----------
!
!  --> N  D'UNITE LOGIQUE ASSOCIE AUX FICHIERS
!-----------------------------------------------------------------------
    integer :: i, imes, ipos, ire1, iret, iunv
    integer :: jconn, jinfo, ndeca, niter, nndec
!-----------------------------------------------------------------------
    call jemarq()
    imes=iunifi('MESSAGE')
!
    inum = 0
!
! --> LECTURE DES MAILLES DANS LE FICHIER UNIVERSEL
!     ---------------------------------------------
    ipos = 0
    it=0
    icp=0
    ino=0
    ico=0
    ibid2=1
    ndeca = 0
    niter = 1000
!
    nsizei=4000
    nsizec=27000
!
    call jeexin('&&PRESUP.INFO.MAILLE', ire1)
    if (ire1 .ne. 0) call jedetr('&&PRESUP.INFO.MAILLE')
    call wkvect('&&PRESUP.INFO.MAILLE', 'V V I', nsizei, jinfo)
    call jeexin('&&PRESUP.CONN.MAILLE', ire1)
    if (ire1 .ne. 0) call jedetr('&&PRESUP.CONN.MAILLE')
    call wkvect('&&PRESUP.CONN.MAILLE', 'V V I', nsizec, jconn)
!
 1  continue
    it=it+1
    nndec=0
!
    do 10 i = 1, niter
!
        read (iunv,fmt='(A)') cbuf
        read (unit=cbuf,fmt='(I6)') ind
!
        if (inum .ne. 0) then
            ico=codgra
        endif
!
        if (ind .eq. -1) goto 99
!
!=== TRAITEMENT DIFFERENT SELON LA VERSION DE SUPERTAB
!    (I-DEAS 4.0 OU I-DEAS 5.0)
!
        if (datset .eq. 71) then
            read(cbuf,'(7I10)') jnum,codgra,codmec,iprop,imat,icol,&
            nbnode
        else if ((datset.eq.82).or.(datset.eq.2431)) then
            read(cbuf,'(3I10)') icol,nbnode,ibid2
            jnum=1
            codgra=1
            iprop=1
            imat=1
            mint(codgra)=0
            mant(codgra)=0
            read(iunv,'(A)') cbuf
        else if (datset.eq.780) then
            read(cbuf,'(8I10)') jnum,coddes,iphyb,iprop,imatb,imat,&
            icol, nbnode
            if (coddes .eq. 11 .or. coddes .eq. 21 .or. coddes .eq. 22 .or. coddes .eq. 23&
                .or. coddes .eq. 24) then
                read(iunv,'(A)') cbuf
            endif
            call cov4v5(coddes, codgra)
        else if (datset.eq.2412) then
            read(cbuf,'(6I10)') jnum,coddes,iprop,imat,icol,nbnode
            if (coddes .eq. 11 .or. coddes .eq. 21 .or. coddes .eq. 22 .or. coddes .eq. 23&
                .or. coddes .eq. 24) then
                read(iunv,'(A)') cbuf
            endif
            call cov4v5(coddes, codgra)
        endif
!
        if ((datset.eq.82) .or. (datset.eq.2431)) then
            call lect82(iunv, nod82, nbnode, jnum)
        else
            call lecelt(iunv, maxnod, nbtyma, indic, permut,&
                        codgra, node, nbnode)
        endif
! --> RECHERCHE DU N MIN ET N MAX POUR UNE CATEGORIE DE MAILLE
!
        inum = inum + 1
        if (ico .gt. 0 .and. ico .eq. codgra) then
            mint(codgra)=min(jnum,mint(codgra))
            mant(codgra)=max(jnum,mant(codgra))
        else
            mint(codgra)=jnum
            mant(codgra)=jnum
        endif
        nbmail(codgra) = nbmail(codgra) + 1
!
! --> ECRITURE DES MAILLES DANS LE FICHIER BUFFER IMA
!
        if ((datset.eq.82) .or. (datset.eq.2431)) then
!
            nn=0
            do 220 ino = 1, 2*(jnum-1)
                if (nod82(ino) .ne. 0) then
                    nn=nn+1
                endif
220          continue
!
            if ((2*nn) .gt. nsizec) then
                nsizec=4*nn
                call juveca('&&PRESUP.CONN.MAILLE', nsizec)
                call jeveuo('&&PRESUP.CONN.MAILLE', 'E', jconn)
            endif
!
            if ((ndeca+nn*4) .gt. nsizei) then
                nsizei=2*(ndeca+nn*4)
                call juveca('&&PRESUP.INFO.MAILLE', nsizei)
                call jeveuo('&&PRESUP.INFO.MAILLE', 'E', jinfo)
            endif
!
            icp=0
            do 22 ino = 1, 2*(jnum-1)
                if (nod82(ino) .ne. 0) then
                    zi(jconn-1+ino) = nod82(ino)
                    zi(jconn-1+ino+1) = nod82(ino+1)
                    zi(jinfo-1+ndeca+(icp)*4+1) = icp+nbmail(codgra)
                    zi(jinfo-1+ndeca+(icp)*4+2) = codgra
                    zi(jinfo-1+ndeca+(icp)*4+3) = 2
                    zi(jinfo-1+ndeca+(icp)*4+4) = icol
                    icp=icp+1
                endif
22          continue
            nbmail(codgra) = jnum + nbmail(codgra)-2
            ipos= ipos+nbnode
            mint(codgra)=1
            mant(codgra)=jnum-1
            inum= jnum-1
            nndec=nndec+4*icp
!
        else
            if ((nbnode+ipos) .gt. nsizec) then
                nsizec=2*(nbnode+ipos)
                call jeexin('&&PRESUP.CONN.MAILLE', iret)
                call juveca('&&PRESUP.CONN.MAILLE', nsizec)
                call jeveuo('&&PRESUP.CONN.MAILLE', 'E', jconn)
            endif
!
            if ((ndeca+niter*4) .gt. nsizei) then
                nsizei=2*(ndeca+niter*4)
                call juveca('&&PRESUP.INFO.MAILLE', nsizei)
                call jeveuo('&&PRESUP.INFO.MAILLE', 'E', jinfo)
            endif
!
            zi(jinfo-1+ndeca+(i-1)*4+1) = jnum
            zi(jinfo-1+ndeca+(i-1)*4+2) = codgra
            zi(jinfo-1+ndeca+(i-1)*4+3) = nbnode
            zi(jinfo-1+ndeca+(i-1)*4+4) = icol
            do 21 ino = 1, nbnode
                zi(jconn-1+ipos+ino) = node(ino)
21          continue
            ipos= ipos+nbnode
            nndec=nndec+4
        endif
!
!
!
10  end do
    ndeca= ndeca+nndec
!
    goto 1
99  continue
    imes = iunifi('MESSAGE')
    write (imes,*) 'NOMBRE DE MAILLES :',inum
    call jedema()
end subroutine
