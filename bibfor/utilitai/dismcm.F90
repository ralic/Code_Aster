subroutine dismcm(questi, nomobz, repi, repkz, ierd)
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
    implicit none
!     --     DISMOI(CHAM_MATER)
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismca.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jelstc.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    integer :: repi, ierd
    character(len=*) :: questi
    character(len=*) :: nomobz, repkz
    character(len=32) :: repk
    character(len=8) :: nomob
! ----------------------------------------------------------------------
!     IN:
!       QUESTI : TEXTE PRECISANT LA QUESTION POSEE
!       NOMOBZ : NOM D'UN OBJET DE TYPE NUM_DDL
!     OUT:
!       REPI   : REPONSE ( SI ENTIERE )
!       REPKZ  : REPONSE ( SI CHAINE DE CARACTERES )
!       IERD   : CODE RETOUR (0--> OK, 1 --> PB)
!
! ----------------------------------------------------------------------
!
!
    character(len=8) :: mater, nomf, novarc
    character(len=10) :: nomrc
    character(len=16) :: ktyp
    character(len=24) :: quest2, nomobj(100)
    character(len=19) :: nomcar2
    logical :: trouve
    integer :: nbmax, izone, i
!-----------------------------------------------------------------------
    integer :: ianorc, iaobj, iaprol, iavale, iavalk, if, ii
    integer :: iii, imax, irc, iret, jdesc, lonobj, n
    integer :: n1, nbrc, nbzone, nc, nf, nmat, nr
    integer :: n2,   nbvarc, nedit, kvarc, kedit
    character(len=8), pointer :: cvrcvarc(:) => null()
    character(len=16), pointer :: vale(:) => null()
!
!-----------------------------------------------------------------------
    parameter(nbmax=30)
!
    call jemarq()
    repk = ' '
    repi = 0
    ierd = 0
!
    nomob=nomobz
!
!
    if (questi .eq. 'NOM_MAILLA') then
!     -----------------------------------
        call dismca(questi, nomob//'.CHAMP_MAT', repi, repk, ierd)
!
!
        elseif (questi.eq.'EXI_AMOR_ALPHA' .or. questi.eq.'EXI_AMOR_NOR'&
    .or.questi.eq.'EXI_AMOR_TAN' .or. questi.eq.'EXI_AMOR_HYST'&
    .or.questi.eq.'EXI_AMOR_BETA') then
!     ---------------------------------------------------------------
        call jeveuo(nomob//'.CHAMP_MAT .VALE', 'L', iavale)
        call jelira(nomob//'.CHAMP_MAT .VALE', 'LONMAX', nmat)
        call jeveuo(nomob//'.CHAMP_MAT .DESC', 'L', jdesc)
        trouve=.false.
        quest2=questi
        nbzone=zi(jdesc+2)
!
        do 40 izone = 1, nbzone
            do 30 imax = 1, nbmax
                i=(izone-1)*nbmax+imax
                mater=zk8(iavale-1+i)
                if (mater .eq. ' ') goto 40
                if (mater .eq. 'TREF=>') goto 40
!
                call jelstc('G', mater, 1, 100, nomobj,&
                            n)
                if (n .lt. 0) then
                    call utmess('F', 'UTILITAI_54')
                endif
                do 20,ii=1,n
                if (nomobj(ii)(20:24) .eq. '.VALK') then
                    call jeveuo(nomobj(ii), 'L', iaobj)
                    call jelira(nomobj(ii), 'LONMAX', lonobj)
                    do 10,iii=1,lonobj
                    if (zk8(iaobj-1+iii) .eq. quest2(5:12)) then
                        trouve=.true.
                        goto 50
!
                    endif
10                  continue
                endif
20              continue
30          continue
40      continue
50      continue
        repk='NON'
        if (trouve) repk='OUI'
!
!
    else if (questi.eq.'EXI_ANISO') then
!     -----------------------------------
        repk='NON'
        call jeveuo(nomob//'.CHAMP_MAT .VALE', 'L', iavale)
        call jelira(nomob//'.CHAMP_MAT .VALE', 'LONMAX', nmat)
        call jeveuo(nomob//'.CHAMP_MAT .DESC', 'L', jdesc)
        nbzone=zi(jdesc+2)
!
        do 80 izone = 1, nbzone
            do 70 imax = 1, nbmax
                i=(izone-1)*nbmax+imax
                mater=zk8(iavale-1+i)
                if (mater .eq. ' ') goto 80
                if (mater .eq. 'TREF=>') goto 80
!
                call jeveuo(mater//'.MATERIAU.NOMRC', 'L', ianorc)
                call jelira(mater//'.MATERIAU.NOMRC', 'LONMAX', nbrc)
                do 60,irc=1,nbrc
                nomrc=zk16(ianorc-1+irc)(1:10)
                if (nomrc .eq. 'ELAS_COQUE') then
                    repk='OUI'
                    goto 90
!
                else if (nomrc.eq.'THER_COQUE') then
                    repk='OUI'
                    goto 90
!
                else if (nomrc.eq.'ELAS_ORTH') then
                    repk='OUI'
                    goto 90
!
                else if (nomrc.eq.'THER_ORTH') then
                    repk='OUI'
                    goto 90
!
                else if (nomrc.eq.'ELAS_COQMU') then
                    repk='OUI'
                    goto 90
!
                else if (nomrc.eq.'THER_COQMU') then
                    repk='OUI'
                    goto 90
!
                endif
60              continue
70          continue
80      continue
90      continue
!
!
    else if (questi.eq.'THER_F_INST') then
!     --------------------------------------
        repk='NON'
        call jeveuo(nomob//'.CHAMP_MAT .VALE', 'L', iavale)
        call jelira(nomob//'.CHAMP_MAT .VALE', 'LONMAX', nmat)
        call jeveuo(nomob//'.CHAMP_MAT .DESC', 'L', jdesc)
        nbzone=zi(jdesc+2)
!
        do 140 izone = 1, nbzone
            do 130 imax = 1, nbmax
                i=(izone-1)*nbmax+imax
                mater=zk8(iavale-1+i)
                if (mater .eq. ' ') goto 140
                if (mater .eq. 'TREF=>') goto 140
!
                call jeveuo(mater//'.MATERIAU.NOMRC', 'L', ianorc)
                call jelira(mater//'.MATERIAU.NOMRC', 'LONMAX', nbrc)
                do 110,irc=1,nbrc
                nomrc=zk16(ianorc-1+irc)(1:10)
!
!            -- SI LE MATERIAU EST ISSU DE LA COMMANDE DEFI_COQU_MULT :
                if (nomrc(5:10) .eq. '_COQMU') goto 120
!
                if (nomrc(1:4) .ne. 'THER') goto 110
                call jeveuo(mater//'.'//nomrc//'.VALK', 'L', iavalk)
                call jelira(mater//'.'//nomrc//'.VALK', 'LONUTI', n1)
                call jelira(mater//'.'//nomrc//'.VALR', 'LONUTI', nr)
                call jelira(mater//'.'//nomrc//'.VALC', 'LONUTI', nc)
                nf=(n1-nr-nc)/2
                do 100,if=1,nf
                nomf=zk8(iavalk-1+nr+nc+nf+if)
                call jeveuo(nomf//'           .PROL', 'L', iaprol)
                if (zk24(iaprol-1+1) .eq. 'NAPPE') then
!              -- CAS D'UNE FONCTION A 2 VARIABLES :
                    if (zk24(iaprol-1+3) .eq. 'INST') repk='OUI'
                    if (zk24(iaprol-1+7) .eq. 'INST') repk='OUI'
                else
!              -- CAS D'UNE FONCTION A 1 VARIABLE :
                    if (zk24(iaprol-1+3) .eq. 'INST') repk='OUI'
                endif
100              continue
110              continue
120              continue
130          continue
140      continue
!
!
!     -- CETTE QUESTION N'EXISTE PLUS. IL NE FAUT PAS L'UTILISER.
!        JE LA CONSERVE JUSTE LE TEMPS DE FAIRE MA RESTIT LA MEME
!        SEMAINE QUE SEBASTIEN MEUNIER QUI MODIFIE VECTME.F
    else if (questi.eq.'ELAS_F_TEMP') then
!     --------------------------------------
        repk='???'
        repi=-99999
!
!
    else if (questi.eq.'ELAS_FO') then
!     --------------------------------------
        repk='NON'
        call jeveuo(nomob//'.CHAMP_MAT .VALE', 'L', iavale)
        call jelira(nomob//'.CHAMP_MAT .VALE', 'LONMAX', nmat)
        call jeveuo(nomob//'.CHAMP_MAT .DESC', 'L', jdesc)
        nbzone=zi(jdesc+2)
!
        do 190 izone = 1, nbzone
            do 180 imax = 1, nbmax
                i=(izone-1)*nbmax+imax
                mater=zk8(iavale-1+i)
                if (mater .eq. ' ') goto 190
                if (mater .eq. 'TREF=>') goto 190
!
                call jeveuo(mater//'.MATERIAU.NOMRC', 'L', ianorc)
                call jelira(mater//'.MATERIAU.NOMRC', 'LONMAX', nbrc)
                do 160,irc=1,nbrc
                nomrc=zk16(ianorc-1+irc)(1:10)
!
!            -- SI LE MATERIAU EST ISSU DE LA COMMANDE DEFI_COQU_MULT :
                if (nomrc(5:10) .eq. '_COQMU') goto 170
!
                if (nomrc(1:4) .ne. 'ELAS') goto 160
                call jeveuo(mater//'.'//nomrc//'.VALK', 'L', iavalk)
                call jelira(mater//'.'//nomrc//'.VALK', 'LONUTI', n1)
                call jelira(mater//'.'//nomrc//'.VALR', 'LONUTI', nr)
                call jelira(mater//'.'//nomrc//'.VALC', 'LONUTI', nc)
                nf=(n1-nr-nc)/2
                do 150,if=1,nf
                nomf=zk8(iavalk-1+nr+nc+nf+if)
                call jeveuo(nomf//'           .PROL', 'L', iaprol)
                if (zk24(iaprol-1+1) .eq. 'CONSTANT') then
!                  -- CAS D'UNE FONCTION CONSTANTE :
                else
!                  -- CAS D'UNE FONCTION VARIABLE :
                    repk='OUI'
                endif
150              continue
160              continue
170              continue
180          continue
190      continue
!
!
    else if (questi.eq.'EXI_VARC') then
!     --------------------------------------
        repk='NON'
        call jeexin(nomob//'.CVRCVARC', iret)
        if (iret .ne. 0) then
            repk='OUI'
        endif
!
!
    else if (questi.eq.'VARC_F_INST') then
!     --------------------------------------
        repk='NON'
        call jeexin(nomob//'.CVRCVARC', iret)
        if (iret .ne. 0) then
            call jeveuo(nomob//'.CVRCVARC', 'L', vk8=cvrcvarc)
            call jelira(nomob//'.CVRCVARC', 'LONMAX', nbvarc)
            do kvarc=1,nbvarc
                novarc=cvrcvarc(kvarc)
                nomcar2=nomob//'.'//novarc
                nomcar2=nomcar2(1:17)//'.2'
                call jeveuo(nomcar2//'.DESC', 'L', jdesc)
                nedit=zi(jdesc-1+3)
                call jeveuo(nomcar2//'.VALE', 'L', vk16=vale)
                call jelira(nomcar2//'.VALE', 'LONMAX', n1)
                n2=n1/nedit
                ASSERT(n1.eq.nedit*n2)
                do kedit=1,nedit
                    ktyp=vale((kedit-1)*n2+2)
                    ASSERT(ktyp.eq.'EVOL' .or.ktyp.eq.'CHAMP')
                    if (ktyp.eq.'EVOL') then
                        repk='OUI'
                        goto 200
                    endif
                enddo
            enddo
        endif
200     continue
!
!
    else
!     --------------------------------------
        ierd=1
    endif
!
    repkz=repk
    call jedema()
end subroutine
