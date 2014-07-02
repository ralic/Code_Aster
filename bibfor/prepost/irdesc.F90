subroutine irdesc(ifi, nbno, prno, nueq, nec,&
                  dg, ncmpmx, vale, nomcmp, titr,&
                  nomnoe, nomsd, nomsym, ir, numnoe,&
                  lmasu)
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/ecrtes.h"
#include "asterfort/exisdg.h"
#include "asterfort/irgags.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/lxliis.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: ifi, nbno, nueq(*), prno(*), nec, dg(*), ncmpmx
    integer :: ir, numnoe(*)
    complex(kind=8) :: vale(*)
    character(len=*) :: nomcmp(*)
    character(len=*) :: titr, nomnoe(*), nomsd, nomsym
    aster_logical :: lmasu
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
!        ECRITURE D'UN CHAM_NO SUR FICHIER UNIVERSEL, DATASET TYPE 55
!        A VALEURS COMPLEXES
!      ENTREE:
!         IFI   : UNITE LOGIQUE DU FICHIER UNIVERSEL
!                NBNO  : NOMBRE DE NOEUDS DU LIGREL ( DU MAILLAGE)
!         PRNO  : PROFIL-NOEUDS
!         NUEQ  : OBJET .NUEQ DU PROF_CHNO
!         NEC   : NOMBRE D'ENTIERS-CODES
!         DG    : ENTIERS CODES
!         NCMPMX: NOMBRE MAXI DE CMP DE LA GRANDEUR
!         VALE  : VALEURS DU CHAM_NO
!         NOMCMP: NOMS DES CMP (9 MAXI)
!         TITR  : 1 LIGNES DE TITRE
!         NOMNOE: NOMS DES NOEUDS
!         NOMSD : NOMS DU RESULTAT
!         NOMSYM: NOM SYMBOLIQUE
!         IR    : NUMERO D'ORDRE DU CHAMP
!         NUMNOE: NUMEROS DES NOEUDS
!         LMASU : INDIQUE SI MAILLAGE SUPERTAB  .TRUE. MAILLAGE SUPERTAB
!
! --------------------------------------------------------------------
!     ------------------------------------------------------------------
    character(len=8) :: nocmp
    character(len=24) :: nomst
    character(len=80) :: entete(10), titre, texte
    integer :: nbchs
    integer :: impre, iente, iutil
    aster_logical :: afaire, lcmp
!
!
!  --- INITIALISATIONS ----
!
!-----------------------------------------------------------------------
    integer :: i, ic, ichs, icmp, icms
    integer :: icmsup, icompt, icp, icval, idebu, iec, ier
    integer :: ifin, inno, ino, iret, irval
    integer :: ival, jmax, jtitr, ncmp
    integer, pointer :: ipcmps(:) => null()
    aster_logical, pointer :: ltabl(:) => null()
    integer, pointer :: nbcmps(:) => null()
    character(len=8), pointer :: nomchs(:) => null()
    character(len=8), pointer :: nomgds(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
    AS_ALLOCATE(vk8=nomgds, size=ncmpmx)
    AS_ALLOCATE(vk8=nomchs, size=ncmpmx)
    AS_ALLOCATE(vi=nbcmps, size=ncmpmx)
    AS_ALLOCATE(vi=ipcmps, size=ncmpmx*ncmpmx)
    AS_ALLOCATE(vl=ltabl, size=ncmpmx)
!
    nomst= '&&IRECRI.SOUS_TITRE.TITR'
    call jeveuo(nomst, 'L', jtitr)
    titre = zk80(jtitr)
    do 1 i = 1, ncmpmx
        ltabl(i)=.false.
  1 end do
!
! --- ALLOCATION DES TABLEAUX DE TRAVAIL ----
!
    call jeexin('&&IRDESC.VALR', iret)
    if (iret .ne. 0) call jedetr('&&IRDESC.VALR')
    call wkvect('&&IRDESC.VALR', 'V V R', ncmpmx, irval)
    call jeexin('&&IRDESC.VALC', iret)
    if (iret .ne. 0) call jedetr('&&IRDESC.VALC')
    call wkvect('&&IRDESC.VALC', 'V V R', ncmpmx, icval)
! ---- RECHERCHE DES GRANDEURS SUPERTAB -----
!
    call irgags(ncmpmx, nomcmp, nomsym, nbchs, nomchs,&
                nbcmps, nomgds, ipcmps)
!
! ---- BOUCLE SUR LES DIVERSES GRANDEURS SUPERTAB ----
    do 10 ichs = 1, nbchs
        if (ichs .gt. 1) then
            afaire=.false.
            do 2 icp = 1, nbcmps(ichs)
                afaire = ( afaire .or. ltabl(ipcmps((ichs-1)* ncmpmx+icp)) )
  2         continue
            if (.not. afaire) goto 10
        endif
        iente = 1
        impre = 0
        lcmp=.false.
        call ecrtes(nomsd, titr, nomgds(ichs), ir, 'NOEU',&
                    nbcmps(ichs), 5, entete, lcmp)
        idebu=1
        entete(4) = ' '
        texte = ' '
        do 5 icp = 1, nbcmps(ichs)
            nocmp = nomcmp(ipcmps((ichs-1)*ncmpmx+icp))
            iutil = lxlgut(nocmp)
            ifin = idebu+iutil
            texte(idebu:ifin) = nocmp(1:iutil)//' '
            idebu = ifin+1
  5     continue
        iutil = lxlgut(texte)
        jmax = lxlgut(titre)
        jmax = min(jmax,(80-iutil-2))
        entete(4)= titre(1:jmax)//' - '//texte(1:iutil)
        do 11 inno = 1, nbno
            ino = numnoe(inno)
            do 17 iec = 1, nec
                dg(iec)=prno((ino-1)*(nec+2)+2+iec)
 17         continue
!
!              NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
!              IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
            ival = prno((ino-1)* (nec+2)+1)
            ncmp = prno((ino-1)* (nec+2)+2)
            if (ncmp .eq. 0) goto 11
!
            do 25 ic = 1, nbcmps(ichs)
                zr(irval-1+ic) = 0.0d0
                zr(icval-1+ic) = 0.0d0
 25         continue
            icompt = 0
            do 12 icmp = 1, ncmpmx
                if (exisdg(dg,icmp)) then
                    if (ichs .eq. 1) ltabl(icmp)= .true.
                    icompt = icompt + 1
                    do 13 icms = 1, nbcmps(ichs)
                        icmsup = ipcmps((ichs-1)*ncmpmx+icms)
                        if (icmp .eq. icmsup) then
                            impre = 1
                            zr(irval-1+icms)=dble(vale(nueq(ival-1+&
                            icompt)))
                            zr(icval-1+icms)=dimag(vale(nueq(ival-1+&
                            icompt)))
                            goto 12
                        endif
 13                 continue
                endif
 12         continue
!
            if (impre .eq. 1) then
                if (iente .eq. 1) then
                    write(ifi,'(A80)') (entete(i),i=1,10)
                    iente=0
                endif
                if (lmasu) then
                    call lxliis(nomnoe(inno)(2:8), ino, ier)
                endif
                write (ifi,'(I10,5X,A,A)') ino,'% NOEUD ',nomnoe(inno)
                write (ifi,'(6(1PE13.5))') (zr(irval-1+i), zr(icval-1+&
                i),i=1,nbcmps(ichs))
                impre=0
            endif
 11     end do
        if (iente .eq. 0) write (ifi,'(A)') '    -1'
 10 end do
    call jedetr('&&IRDESC.VALR')
    call jedetr('&&IRDESC.VALC')
    AS_DEALLOCATE(vk8=nomgds)
    AS_DEALLOCATE(vk8=nomchs)
    AS_DEALLOCATE(vi=nbcmps)
    AS_DEALLOCATE(vi=ipcmps)
    AS_DEALLOCATE(vl=ltabl)
    call jedema()
end subroutine
